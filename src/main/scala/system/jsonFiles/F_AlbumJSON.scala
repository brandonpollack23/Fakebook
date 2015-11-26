package system.jsonFiles

import java.util.Date

import spray.json._
import DefaultJsonProtocol._
import graphnodes._
import MyJsonProtocol._

object F_AlbumJSON extends DefaultJsonProtocol
{
  def write(date: Date) = {
    JsArray(JsString(date.getMonth.toString), JsString(date.getDate.toString), JsString((date.getYear + 1900).toString))
  }

  def write(bigInt: BigInt) = {
    JsString(bigInt.toString(16))
  }

  def getJSON(album: F_Album) : JsValue ={

    album.toJson                   //This may not work due to use of Date data type in object,
    //we may need to have own jsonProtocol implemented for conversion

  }

}