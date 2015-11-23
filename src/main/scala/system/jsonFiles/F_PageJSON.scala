package system.jsonFiles

import spray.json._
import graphnodes._
//import com.oracle.javafx.jmx.json.JSONDocument    //in case return type is JSONDocument insted of JsValue

object F_PageJSON extends DefaultJsonProtocol
{

  def getJSON(page: F_Page) : JsValue ={

    page.toJson        //This may not work due to use of Date data type in object,
    //we may need to have own jsonProtocol implemented for conversion

  }

}
