package system.jsonFiles

import spray.json._
import graphnodes._
import MyJsonProtocol._

object F_UserJSON //extends DefaultJsonProtocol
{

  def getJSON(user: F_User) : JsValue ={

    user.toJson

  }

}