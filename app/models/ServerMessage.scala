package models

import play.api.libs.json.JsValue
import play.api.libs.iteratee.Enumerator

case class Position(x: Int, y: Int)
case class Mappa(mapa :List[Int])

sealed abstract class ServerMessage
case class Connected	(enumerator:Enumerator[JsValue])	   		extends ServerMessage
case class CannotConnect(message: String)                                  extends ServerMessage
case class Join         (username: String)				  extends ServerMessage
case class Move         (username: String, xy: Int, dir: Int)                extends ServerMessage
case class Mapa   	(username: String)                                extends ServerMessage
case class Bomb		()	  				  extends ServerMessage
case class UpdateScore  (username: String, score: Int)                        extends ServerMessage
case class Quit         (username: String)                                    extends ServerMessage
