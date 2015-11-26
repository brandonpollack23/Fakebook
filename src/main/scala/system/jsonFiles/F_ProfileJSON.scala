package system.jsonFiles

import spray.json._
import graphnodes._
import MyJsonProtocol._

object F_ProfileJSON extends DefaultJsonProtocol
{

  def getJSON(profile: F_UserProfile) : JsValue ={

    profile.toJson

  }

}
