package system.jsonFiles

import spray.json._
import graphnodes._
import MyJsonProtocol._

object F_PostJSON //extends DefaultJsonProtocol
 {

  def getJSON(post: F_Post) ={

    post.toJson.compactPrint

  }

}
