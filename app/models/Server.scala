package models

import akka.actor.{ActorRef, Actor, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee._
import play.api.libs.json._
import play.libs.Akka
import play.libs.Time
import scala.collection.immutable.Map
import scala.util.Random



object Server {
  implicit val timeout = Timeout(1 second)
  lazy val default = Akka.system.actorOf(Props[Server])

 def join(username:String):scala.concurrent.Future[(Iteratee[JsValue,_],Enumerator[JsValue])] = {

    (default ? Join(username)).map {
      case Connected(enumerator) => 
        val iteratee = Iteratee.foreach[JsValue]{event =>
          default ! Mapa(username)}
        .mapDone { _ =>
          default ! Quit(username)
        }
        (iteratee,enumerator)

      case _ =>
        val iteratee   = Done[JsValue,Unit]((),Input.EOF)
        val enumerator =
          Enumerator[JsValue](Json.obj("error" -> "Server cannot create unique user name")) >>>
          Enumerator.enumInput(Input.EOF)
        (iteratee,enumerator)
    }
  }
}

class Server extends Actor {
  var playersSet = collection.mutable.Map.empty[String, Position]
  val random = new Random
  //var playersSet = Map.empty[String, Position]
  
  //zamiast 'data'
 def random2DimArray(dim1: Int, dim2: Int) = Array.fill(dim1, dim2){random.nextInt(3)}
 val data = random2DimArray(12, 15)
  var scoreboardMap = collection.mutable.Map.empty[String, Int]
  var userBombLimit = collection.mutable.Map.empty[String, Int]
  
  def bomb = userBombLimit 
  def players = playersSet
  def scoreboard = scoreboardMap
  
    //wartość wyliczająca odbiorców, kanał nadawania do wszystkich
  val (playersEnumerator, channel) = Concurrent.broadcast[JsValue]
  
  def receive = {
      
   
    case Join(username) => {
      if(players.contains(username)) {
        sender ! CannotConnect("This username is already used")
      } else {
        playersSet += (username -> randomPosition)
        addBombToUser(username)
        addUserToScoreboard(username)
        sender ! Connected(playersEnumerator)
      }
    }

    //dostaje plansze
    case Mapa(username) => {
      sender ! (Json.arr(Json.obj(
          "command"-> "mapa",
          "data" -> (data),
          "players" -> (players.toString),
          "scoreboard" -> Json.arr(scoreboard.toString)
          ))) 
    } 
        
        
    case Move(username, xy: Int, dir: Int ) => {
      move(username, xy, dir)
      val pos = playersSet(username)
      channel.push(Json.obj("command" -> "move", 
    		  				"username" -> username,
    		  				"pos" -> Json.arr(pos.x, pos.y)))
    }
    
    case Bomb() => {
      
    }
   



    case Quit(username) => 
      playersSet = playersSet - username
      removeUserAndBombs(username)
    
  }
  
//komunikaty serwera  
  def death(username: String) = {
    
  }

   
  def reduceBomb(username: String) = {
    userBombLimit(username) = userBombLimit(username) - 1
    }                                             
    

  def haveBomb(username: String): Boolean = {
     if (bomb.contains(username) && bomb(username) > 0) true else false
   }                                           
   
  
  def bombCounter(username: String, pos: Position) = {
     if (haveBomb(username)) { Thread.sleep(2000)
      channel.push(Json.arr(Json.obj("comand" -> "fire",
          "player" -> username,
           "x" -> pos.x ,"y" -> pos.y)))
           reduceBomb(username)
           updateBombs(username)
           }
           bomb(username)
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
   
  def randomPosition :Position = {
   val pos = new Position(random.nextInt(3), random.nextInt(5))
   if (data(pos.x)(pos.y).equals(1)) {
     val keyAsValue = players.values
     if (!keyAsValue.eq(pos)) pos else randomPosition
     } else randomPosition
  }
  
  def addUserToScoreboard(username: String) = 
    scoreboardMap += (username -> 0)
  
  def removeUserFromScoreboard(username: String) = {
    scoreboardMap -= (username)
  }
  
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
      	if(dir.equals(1) && !position.y.equals(5)){
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
      	if(dir.equals(1) && !position.x.equals(5)){
          if(!playersSet.valuesIterator.contains(position.x + dir, position.y) &&
          data(position.x)(position.y + dir).equals(1)){
          playersSet.update(username, Position(position.x + dir, position.y))
          }
        }
      }
     }

  } 
  
  

}