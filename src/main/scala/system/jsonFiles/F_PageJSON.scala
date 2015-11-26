package system.jsonFiles

import spray.json._
import graphnodes._
import MyJsonProtocol._

object F_PageJSON extends DefaultJsonProtocol
{

  def getJSON(page: F_Page) : JsValue ={

    page.toJson

  }

}
