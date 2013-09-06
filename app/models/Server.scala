package models

import akka.actor.{ActorRef, Actor, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import play.api.libs.functional.syntax._
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.json.Json.JsValueWrapper
import play.libs.Akka
import play.libs.Time
import scala.collection.immutable.Map
import scala.util.Random


object ConsLogger {
  
  def apply(logger: ActorRef) {
    
    // Create an Iteratee that logs all messages to the console.
    val loggerIteratee = Iteratee.foreach[JsValue](event => println(event.toString))
    
    implicit val timeout = Timeout(1 second)
    // Make the robot join the room
    logger ? (Join("ConsLogger")) map {
      case Connected(robotChannel) => 
        // Apply this Enumerator on the logger.
        robotChannel |>> loggerIteratee
    }
    
  }
  
}



object Server {
  implicit val timeout = Timeout(1 second)
 // lazy val default = Akka.system.actorOf(Props[Server])

    lazy val default = {
    val logActor = Akka.system.actorOf(Props[Server])
    
    // Create a bot user (just for fun)
    ConsLogger(logActor)
    
    logActor
  }

 def join(username:String):scala.concurrent.Future[(Iteratee[JsValue,_],Enumerator[JsValue])] = {

    (default ? Join(username)).map {
      case Connected(enumerator) => 
        val iteratee = Iteratee.foreach[JsValue]{event =>
          default ! Event(username, event)}
        .mapDone { _ =>
          default ! Quit(username)
        }
        (iteratee,enumerator)

       case CannotConnect(error) => 
      
        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue,Unit]((),Input.EOF)

        // Send an error and close the socket
        val enumerator =  Enumerator[JsValue](JsObject(Seq("error" -> JsString(error))))
        .andThen(Enumerator.enumInput(Input.EOF))
        
        (iteratee,enumerator)
    }
  }
}

class Server extends Actor {
  
  var members = Map.empty[String, Concurrent.Channel[JsValue]]
  var playersSet = collection.mutable.Map.empty[String, Position]
  val random = new Random
  //var playersSet = Map.empty[String, Position]
  
  //zamiast 'data'
 def random2DimArray(dim1: Int, dim2: Int) = Array.fill(dim1, dim2){random.nextInt(2)}
 val data = random2DimArray(25, 40)
  var scoreboardMap = collection.mutable.Map.empty[String, Int]
  var userBombLimit = collection.mutable.Map.empty[String, Int]
  
  def bomb = userBombLimit 
  def players = playersSet
  def scoreboard = scoreboardMap
  
    //wartość wyliczająca odbiorców, kanał nadawania do wszystkich
  //val (playersEnumerator, channel) = Concurrent.broadcast[JsValue]
  
  def receive = {
      
   
    case Join(username) => {
      if(players.contains(username)) {
        sender ! CannotConnect("This username is already used")
      } else {
        playersSet += (username -> randomPosition)
        addBombToUser(username)
        addUserToScoreboard(username)
        val enumerator = Concurrent.unicast[JsValue]{
             c => members = members + (username -> c)}
        sender ! Connected(enumerator)
        Thread.sleep(1000)
        self ! NotifyJoin(username)
      }
    }


    case NotifyJoin(username) => {
      for(channel <- members.values){
        channel.push(Json.obj(
          "command" -> "join",
          "username" -> (username),
          "pos" -> Json.toJson(players.get(username))
          )
        )
      }
    }

    case Event(username, event) => {
      def getCommand(command: String) = (event \ command).as[String]
      def getMove(command: String) = (event \ command).as[Int]

      getCommand("command")match{
        case "mapa" => self ! Mapa(username)
        case "move" => self ! Move(username, getMove("xy"), getMove("dir") )
        case "bomb" => self ! Bomb(username)
        case "quit" => self ! Quit(username)
        case _ => println("Unnable to parse message %s".format(event.toString))
      }
    }

    //dostaje plansze
        
 case Mapa(username) => {
      for (channel <- members.get(username)){
       channel.push(Json.obj(
          "command"-> "mapa",
          "data" -> (data),
          "players" ->  Json.obj(players.map{case (s, o) =>
              val ret:(String, JsValueWrapper) = o match {
              case pos:Position => s -> Json.toJson(pos)
              }
              ret
              }.toSeq:_*),
          "scoreboard" -> Json.obj(scoreboard.map{case(k, v) => 
                            val ret:(String, JsValueWrapper) = v match {
                              case i:Int => k -> Json.arr(i)
                              }
                              ret}.toSeq:_*)  
          ))
      } 
    }


    case Move(username, xy: Int, dir: Int ) => {
      move(username, xy, dir)
      val pos = playersSet(username)
      for(channel <- members.values){
      channel.push(Json.obj(
          "command" -> "move", 
          "username" -> username,
          "pos" -> Json.toJson(pos)
          )
      )
      }
    }
    
    case Bomb(username) => {
      bombCounter(username)
    }
   



    case Quit(username) => {
     for(channel <- members.get(username)){
      channel.push(Json.obj(
        "command" -> "score",
        "value" -> scoreboard.get(username)
        )
      )
     }
      playersSet = playersSet - username
      removeUserAndBombs(username)
      members = members - username
      scoreboardMap = scoreboardMap - username
    }
  }
  
//komunikaty serwera  



  def reduceBomb(username: String) = {
    userBombLimit(username) = userBombLimit(username) - 1
    }                                             
    

  def haveBomb(username: String): Boolean = {
     if (bomb.contains(username) && bomb(username) > 0) true else false
   }                                           
   
  
  def bombCounter(username: String) = {
    val pos = players(username)
     if (haveBomb(username)) { 
      for (channel <- members.values){
        channel.push(Json.obj(
          "command" -> "bomb",
          "pos" -> Json.toJson(pos)
          )
        )
      }
      Thread.sleep(2000)
      for(channel <- members.values){
      channel.push(Json.obj(
        "comand" -> "fire",
        "player" -> username,
         "pos"-> Json.toJson(pos)
        )
      )
    }
           reduceBomb(username)
           updateBombs(username)
           val killed = explosionArea(pos)
           if (!killed.isEmpty){
            updateScoreboard(username, killed.length)
             for(channel <- members.values){
             channel.push(Json.obj(
              "command" -> "death",
              "player" -> Json.toJson(killed)
              )
            )
          }
          for(kill <- killed)
              for(channel <- members.get(kill)){
            channel.push(Json.obj(
        "command" -> "quit",
        "score" -> scoreboard.get(kill)
              )
            )
        
        println(scoreboard.toString)
          }
        }
      }
  }                                              
   
   
  def addBombToUser(username: String) = {
    if(!bomb.contains(username)){
      userBombLimit = userBombLimit + (username -> 5)
    }
  }                                               //> addBombToUser: (username: String)Any
    
  def updateBombs(username: String) = {
    if(bomb(username) == 0){
    Thread.sleep(12000)
    userBombLimit(username) = userBombLimit(username) + 5
    }
  }
  
  def removeUserAndBombs(username: String) = {
    if(bomb.contains(username)){
      userBombLimit -= username
    }
  }
   
  def playersValList = players.values.toList

  def randomPosition :Position = {
   val pos = new Position(random.nextInt(24), random.nextInt(39))
   if (data(pos.x)(pos.y).equals(1)) {
     if (playersValList.contains(pos)) randomPosition else pos
     } else randomPosition
  }
  
  def addUserToScoreboard(username: String) = 
    scoreboardMap += (username -> 0)
    
  def move(username: String, xy: Int , dir: Int) = {
    val position = playersSet(username)
    val moveBackY = {
      //po osi y
      if(xy.equals(0)){
        if(dir.equals(-1) && !position.y.equals(0)){
          if(!playersSet.valuesIterator.contains(position.x, position.y + dir) &&
          data(position.x)(position.y + dir).equals(1)){
          playersSet.update(username, Position(position.x, position.y + dir))
          }
        }
      }
    }
      val moveForwardY = {
      if(xy.equals(0)){
        if(dir.equals(1) && !position.y.equals(39)){
          if(!playersSet.valuesIterator.contains(position.x, position.y + dir) &&
          data(position.x)(position.y + dir).equals(1)){
          playersSet.update(username, Position(position.x, position.y + dir))
          }
        }
      }
    } 
  //po osi x
      val moveBackX = {
      if(xy.equals(1)){
        if(dir.equals(-1) && !position.x.equals(0)){
          if(!playersSet.valuesIterator.contains(position.x + dir, position.y) &&
          data(position.x + dir)(position.y).equals(1)){
          playersSet.update(username, Position(position.x + dir, position.y))
          }
        }
      }
    }
      val moveForwardX = {
      if(xy.equals(1)){
        if(dir.equals(1) && !position.x.equals(24)){
          if(!playersSet.valuesIterator.contains(position.x + dir, position.y) &&
          data(position.x)(position.y + dir).equals(1)){
          playersSet.update(username, Position(position.x + dir, position.y))
          }
        }
      }
     }

  } 
  
  def checkPositions (list: List[Position]): List[String] = {
    val buffer = new collection.mutable.ListBuffer[String]
    val swap = playersSet.map(_.swap)
    for(lis <- list if swap.contains(lis)) {
      buffer += swap(lis)
    }
    var nameList = buffer.toList
    nameList
  }

  def explosionArea(pos: Position) = { 
    val buffer = new collection.mutable.ListBuffer[Position]
    for (i <- pos.x - 2 to pos.x + 2){
      buffer += Position(i , pos.y)
    }
    for (i <- pos.y - 2 to pos.y + 2){
      buffer += Position(pos.x , i)
    }
    val list = buffer.distinct.toList
    checkPositions(list)
  }

   def updateScoreboard(username: String, killed: Int) = {
     scoreboardMap.update(username, scoreboardMap(username) + (10 * killed))
  }

}