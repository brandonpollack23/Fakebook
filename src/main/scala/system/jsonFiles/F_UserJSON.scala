package system.jsonFiles

import spray.json._
import graphnodes._
//import com.oracle.javafx.jmx.json.JSONDocument

object F_UserJSON extends DefaultJsonProtocol
{

  def getJSON(user: F_User) : JsValue ={

    user.toJson

  }

}