package system.jsonFiles

import spray.json._
import graphnodes._
import MyJsonProtocol._

object F_AlbumJSON //extends DefaultJsonProtocol
{
  /*
  def write(date: Date) = {
    JsArray(JsString(date.getMonth.toString), JsString(date.getDate.toString), JsString((date.getYear + 1900).toString))
  }

  def write(bigInt: BigInt) = {
    JsString(bigInt.toString(16))
  }
*/
  def getJSON(album: F_Album) : JsValue ={

    album.toJson

  }

}