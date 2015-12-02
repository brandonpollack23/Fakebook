package util

import java.util.Date

import graphnodes._
import spray.json._

object MyJsonProtocol extends DefaultJsonProtocol {
  //we need to provide (de)serialization methods only for Date, everything else can be implicit
  implicit object DateJsonFormat extends RootJsonFormat[Date] {
    def write(d: Date) = JsObject(
      "month" -> JsNumber(d.getMonth + 1),
      "date" -> JsNumber(d.getDate),
      "year" -> JsNumber(d.getYear + 1900),
      "hour" -> JsNumber(d.getHours),
      "minute" -> JsNumber(d.getMinutes),
      "second" -> JsNumber(d.getSeconds)
    )

    def read(d: JsValue) = {
      d.asJsObject.getFields("month", "date", "year", "hour", "minute", "second") match {
        case Seq(JsNumber(month), JsNumber(date), JsNumber(year), JsNumber(hour), JsNumber(minute), JsNumber(second)) =>
          new Date(year.toInt - 1900, month.toInt - 1, date.toInt, hour.toInt, minute.toInt, second.toInt)

        case _ => throw new DeserializationException("something wrong with deserialization of Date")
      }
    }
  }

  implicit val userFormat = jsonFormat10(F_User.apply)
  implicit val userEFormat = jsonFormat10(F_UserE.apply)
  implicit val profileFormat = jsonFormat7(F_UserProfile.apply)
  implicit val profileEFormat = jsonFormat7(F_UserProfileE.apply)
  implicit val postFormat = jsonFormat6(F_Post.apply)
  implicit val postEFormat = jsonFormat6(F_PostE.apply)
  implicit val pictureFormat = jsonFormat7(F_Picture.apply)
  implicit val pictureEFormat = jsonFormat7(F_PictureE.apply)
  implicit val albumFormat = jsonFormat7(F_Album.apply)
  implicit val albumEFormat = jsonFormat7(F_AlbumE.apply)
  implicit val pageFormat = jsonFormat9(F_Page.apply)

  private def jsByteVector(ba: Array[Byte]) = JsArray(ba.map(JsNumber(_)).toVector)
}