package system.jsonFiles

import spray.json._
import graphnodes._
//import com.oracle.javafx.jmx.json.JSONDocument

object F_ProfileJSON extends DefaultJsonProtocol
{

  def getJSON(profile: F_UserProfile) : JsValue ={

    profile.toJson

  }

}
