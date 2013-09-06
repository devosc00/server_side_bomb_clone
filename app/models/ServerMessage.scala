package models

<<<<<<< HEAD
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.libs.functional._
=======
import play.api.libs.json._
import play.api.libs.iteratee.Enumerator
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.Json.JsValueWrapper
>>>>>>> bc96360d7bbfc58c7c964ca53f2f6f0cbc72b18c

case class Position(x: Int, y: Int)

object Position {
	implicit val posWrite = new Writes[Position]{
    def writes(p: Position): JsValue = {
      Json.arr(p.x, p.y)
    }
  }
<<<<<<< HEAD
} 
=======
}
>>>>>>> bc96360d7bbfc58c7c964ca53f2f6f0cbc72b18c


sealed abstract class ServerMessage
case class Event		(username: String, event: JsValue)			extends ServerMessage
case class Connected	(enumerator:Enumerator[JsValue])	   		extends ServerMessage
<<<<<<< HEAD
case class CannotConnect(message: String)                           extends ServerMessage
case class Join         (username: String)				  			extends ServerMessage
case class Event		(username: String, event: JsValue)			extends ServerMessage
case class Move         (username: String, xy: Int, dir: Int)       extends ServerMessage
case class Mapa   		(username: String)                          extends ServerMessage
case class Bomb			(username: String)	  				  		extends ServerMessage
case class NotifyJoin   (username: String)                        	extends ServerMessage
case class Quit         (username: String)                          extends ServerMessage
=======
case class CannotConnect(message: String)                                  extends ServerMessage
case class Join         (username: String)				  extends ServerMessage
case class Move         (username: String, xy: Int, dir: Int)                extends ServerMessage
case class Mapa   	(username: String)                                extends ServerMessage
case class Bomb		(username: String)	  				  extends ServerMessage
case class NotifyJoin  (username: String)                        extends ServerMessage
case class Quit         (username: String)                                    extends ServerMessage
>>>>>>> bc96360d7bbfc58c7c964ca53f2f6f0cbc72b18c
