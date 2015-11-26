package system.jsonFiles


import spray.json._
import graphnodes._
import MyJsonProtocol._

object F_PostJSON extends DefaultJsonProtocol {

  def getJSON(post: F_Post) : JsValue ={

    post.toJson        //This may not work due to use of Date data type in object,
                      //we may need to have own jsonProtocol implemented for conversion

  }


}
