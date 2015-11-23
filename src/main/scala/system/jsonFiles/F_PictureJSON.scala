package system.jsonFiles

import spray.json._
import graphnodes._
//import com.oracle.javafx.jmx.json.JSONDocument

object F_PictureJSON extends DefaultJsonProtocol
{

  def getJSON(picture: F_Picture) : JsValue ={

    picture.toJson

  }

}
