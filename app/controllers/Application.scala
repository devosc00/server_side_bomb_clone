package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.JsValue

object Application extends Controller {
  
  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  def stream(username: String) = WebSocket.async[JsValue] { implicit request =>
    models.Server.join(username)
	}
}