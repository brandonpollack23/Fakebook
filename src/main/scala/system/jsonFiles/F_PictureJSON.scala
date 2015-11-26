package system.jsonFiles

import spray.json._
import graphnodes._
import MyJsonProtocol._

object F_PictureJSON //extends DefaultJsonProtocol
{

  def getJSON(picture: F_Picture) : JsValue ={

    picture.toJson

  }

}
