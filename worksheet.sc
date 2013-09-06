import play.api.libs.json._
import models.Position
import java.util.Random
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee._



object worksheet {
import scala.collection.mutable.HashMap


var userBombLimit = scala.collection.mutable.Map[String, Int]()
                                                  //> userBombLimit  : scala.collection.mutable.Map[String,Int] = Map()
  var members = Map.empty[String, Concurrent.Channel[JsValue]]
                                                  //> members  : scala.collection.immutable.Map[String,play.api.libs.iteratee.Conc
                                                  //| urrent.Channel[play.api.libs.json.JsValue]] = Map()
  var playersSet = collection.mutable.Map.empty[String, Position]
                                                  //> playersSet  : scala.collection.mutable.Map[String,models.Position] = Map()

  var scoreboardMap = collection.mutable.Map.empty[String, Int]
                                                  //> scoreboardMap  : scala.collection.mutable.Map[String,Int] = Map()
 
  def players = playersSet                        //> players: => scala.collection.mutable.Map[String,models.Position]
  def scoreboard = scoreboardMap                  //> scoreboard: => scala.collection.mutable.Map[String,Int]

  def bomb = userBombLimit                        //> bomb: => scala.collection.mutable.Map[String,Int]
  
  val random = new scala.util.Random              //> random  : scala.util.Random = scala.util.Random@193a66f
  def random2DimArray(dim1: Int, dim2: Int) = Array.fill(dim1, dim2){random.nextInt(2)}
                                                  //> random2DimArray: (dim1: Int, dim2: Int)Array[Array[Int]]
  val data =  random2DimArray(4, 6)               //> data  : Array[Array[Int]] = Array(Array(1, 0, 1, 0, 1, 0), Array(1, 1, 0, 1,
                                                  //|  0, 0), Array(1, 1, 1, 0, 0, 1), Array(1, 1, 0, 0, 1, 0))
    
  def addPlayer(user: String) = playersSet += (user -> randomPosition)
                                                  //> addPlayer: (user: String)scala.collection.mutable.Map[String,models.Position
                                                  //| ]
  def addPlayerToScoreboard(user: String) = scoreboardMap += (user -> 0)
                                                  //> addPlayerToScoreboard: (user: String)scala.collection.mutable.Map[String,Int
                                                  //| ]
  
   def randomPosition :Position = {
   val r = new scala.util.Random
   val pos = new Position(r.nextInt(3), r.nextInt(5))
   if (data(pos.x)(pos.y).equals(1)) {
     val keyAsValue = playersSet.values
     if (keyAsValue.exists(pos => false)) pos else randomPosition
     } else randomPosition
  }                                               //> randomPosition: => models.Position


  def reduceBomb(username: String) = {
    userBombLimit(username) = userBombLimit(username) - 1
    }                                             //> reduceBomb: (username: String)Unit


  def haveBomb(username: String): Boolean = {
     if (bomb.contains(username) && bomb(username) > 0) true else false
   }                                              //> haveBomb: (username: String)Boolean
   
 val swap = players.map(_.swap)                   //> swap  : scala.collection.mutable.Map[models.Position,String] = Map()
  
  def checkPositions (list: List[Position]): List[String] = {
    val buffer = new collection.mutable.ListBuffer[String]
    for(lis <- list if players.valuesIterator.contains(lis)) {
      buffer += swap(lis)
    }
    var nameList = buffer.toList
    nameList
  }                                               //> checkPositions: (list: List[models.Position])List[String]
   
   
  def explosionArea(pos: Position) = {
    val buffer = new collection.mutable.ListBuffer[Position]
    for (i <- pos.x - 2 to pos.x + 2){
      buffer += Position(i , pos.y)
    }
    for (i <- pos.y - 2 to pos.y + 2){
      buffer += Position(pos.x , i)
    }
    val list = buffer.distinct.toList
//    println(list)
    checkPositions(list)
  }                                               //> explosionArea: (pos: models.Position)List[String]
  
  
    def updateScoreboard(username: String) = {
     scoreboardMap.update(username, scoreboardMap(username) + 10)
  }                                               //> updateScoreboard: (username: String)Unit
  
 def bombCounter(username: String) = {
    val pos = players(username)
     if (haveBomb(username)) {
      for (channel <- members.values){
        channel.push(Json.arr(Json.obj(
          "command" -> "bomb",
          "x" -> pos.x,
          "y" -> pos.y)
          )
        )
      }
      Thread.sleep(2000)
      for(channel <- members.values){
      channel.push(Json.arr(Json.obj(
        "comand" -> "fire",
        "player" -> username,
        "x" -> pos.x ,
        "y" -> pos.y)
        )
      )
    }
           reduceBomb(username)
           updateBombs(username)
           val killed = explosionArea(pos)
           for(kill <- killed)
             for(channel <- members.get(kill)){
                updateScoreboard(username)
             channel.push(Json.arr(Json.obj(
              "command" -> "death"
              )
            )
          )
        }

    }
  }                                               //> bombCounter: (username: String)Unit
   
  
  def addBombToUser(username: String) = {
    if(!bomb.contains(username)){
      userBombLimit = userBombLimit + (username -> 5)
    }
  }                                               //> addBombToUser: (username: String)Unit
    
  def updateBombs(username: String) = {
    if(bomb(username) == 0){
    Thread.sleep(1000)
    userBombLimit(username) = userBombLimit(username) + 5
    }
  }                                               //> updateBombs: (username: String)Unit
  
  def removeUserAndBombs(username: String) = {
    if(bomb.contains(username)){
    userBombLimit -= username
    }
  }                                               //> removeUserAndBombs: (username: String)Any-


addPlayer("p ")


}