import play.api.libs.json._
import models.Position
import java.util.Random



object worksheet {
import scala.collection.mutable.HashMap


var userBombLimit = scala.collection.mutable.Map[String, Int]()
                                                  //> userBombLimit  : scala.collection.mutable.Map[String,Int] = Map()

  var playersSet = collection.mutable.Map.empty[String, Position]
                                                  //> playersSet  : scala.collection.mutable.Map[String,models.Position] = Map()
 // val data = Map ("0,0" -> "0", "0,1" -> "1", "0,2" -> "1", "0,3" -> "1")

  var scoreboardMap = collection.mutable.Map.empty[String, Int]
                                                  //> scoreboardMap  : scala.collection.mutable.Map[String,Int] = Map()
 
  def players = playersSet                        //> players: => scala.collection.mutable.Map[String,models.Position]
  def scoreboard = scoreboardMap                  //> scoreboard: => scala.collection.mutable.Map[String,Int]

  def bomb = userBombLimit                        //> bomb: => scala.collection.mutable.Map[String,Int]
  
  val random = new scala.util.Random              //> random  : scala.util.Random = scala.util.Random@13c468a
  def random2DimArray(dim1: Int, dim2: Int) = Array.fill(dim1, dim2){random.nextInt(2)}
                                                  //> random2DimArray: (dim1: Int, dim2: Int)Array[Array[Int]]
  val data =  random2DimArray(4, 6)               //> data  : Array[Array[Int]] = Array(Array(1, 0, 0, 0, 1, 1), Array(0, 0, 1, 1,
                                                  //|  1, 1), Array(0, 1, 0, 0, 1, 0), Array(1, 0, 0, 1, 1, 1))
    
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
     val keyAsValue = players.values
     if (!keyAsValue.eq(pos)) pos else randomPosition
     } else randomPosition
  }                                               //> randomPosition: => models.Position

  def reduceBomb(username: String) = {
    userBombLimit(username) = userBombLimit(username) - 1
    }                                             //> reduceBomb: (username: String)Unit


  def haveBomb(username: String): Boolean = {
     if (bomb.contains(username) && bomb(username) > 0) true else false
   }                                              //> haveBomb: (username: String)Boolean
   
   //zwraca liczbę bomb danego usera
   def bombCounter(username: String, pos: Position) = {
     if (haveBomb(username)) { Thread.sleep(200)
       val msg = Json.arr(Json.obj("comand" -> "fire", "player" -> username,
           "x" -> pos.x ,"y" -> pos.y))
           reduceBomb(username)
           updateBombs(username)
           }
           bomb(username)
   }                                              //> bombCounter: (username: String, pos: models.Position)Int
   
  
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
  }                                               //> removeUserAndBombs: (username: String)Any

   
  def mapa(user: String) = {
  (Json.arr(Json.obj(
          "command"-> "mapa",
          "data" -> (data),
          "players" -> Json.arr(players.toString),
          "scoreboard" -> Json.arr(scoreboard.toString)
          )))
   }                                              //> mapa: (user: String)play.api.libs.json.JsArray


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
      val pos = playersSet(username)
       Json.obj("command" -> "move",
    		  						"username" -> username,
    		  						"pos" -> Json.arr(pos.x, pos.y))
  }                                               //> move: (username: String, xy: Int, dir: Int)play.api.libs.json.JsObject
  
  addPlayer("play" )                              //> res0: scala.collection.mutable.Map[String,models.Position] = Map(play -> Po
                                                  //| sition(2,1))
  addPlayerToScoreboard("play")                   //> res1: scala.collection.mutable.Map[String,Int] = Map(play -> 0)
 players                                          //> res2: scala.collection.mutable.Map[String,models.Position] = Map(play -> Po
                                                  //| sition(2,1))
 move("play",0, -1 )                              //> res3: play.api.libs.json.JsObject = {"command":"move","username":"play","po
                                                  //| s":[2,1]}
  
  players                                         //> res4: scala.collection.mutable.Map[String,models.Position] = Map(play -> Po
                                                  //| sition(2,1))
}