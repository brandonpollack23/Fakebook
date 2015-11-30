package system

import java.util.Date

import graphnodes._
import spray.json._

object MyJsonProtocol extends DefaultJsonProtocol {
  //we need to provide (de)serialization methods only for Date, everything else can be implicit
  implicit object DateJsonFormat extends RootJsonFormat[Date] {
    def write(d: Date) = JsObject(
      "month" -> JsNumber(d.getMonth),
      "day" -> JsNumber(d.getDate),
      "year" -> JsNumber(d.getYear)
    )

    def read(d: JsValue) = {
      d.asJsObject.getFields("month", "day,", "year") match {
        case Seq(JsNumber(month), JsNumber(date), JsNumber(year)) =>
          new Date(year.toInt - 1990, month.toInt, date.toInt)

        case _ => throw new DeserializationException("something wrong with deserialization of Date")
      }
    }
  }

  implicit val userFormat = jsonFormat10(F_User.apply)
  implicit val profileFormat = jsonFormat6(F_UserProfile.apply)
  implicit val postFormat = jsonFormat6(F_Post.apply)
  implicit val pictureFormat = jsonFormat7(F_Picture.apply)
  implicit val albumFormat = jsonFormat7(F_Album.apply)
  implicit val pageFormat = jsonFormat8(F_Page.apply)
}
