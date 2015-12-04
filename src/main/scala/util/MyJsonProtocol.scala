package util

import java.security.{KeyFactory, PublicKey}
import java.security.spec.{X509EncodedKeySpec, RSAPublicKeySpec}
import java.util.Date
import javax.crypto.SecretKey
import javax.crypto.spec.SecretKeySpec

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

  implicit object PublicKeyJsonFormat extends RootJsonFormat[PublicKey] {
    def write(k: PublicKey) = JsString(BigInt(k.getEncoded).toString(16))

    def read(t: JsValue) = t match {
      case JsString(key) => KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(BigInt(key, 16).toByteArray))
      case _ => deserializationError("bigint string to change to RSA public key expected")
    }
  }

  implicit object MyBigIntJsonFormat extends RootJsonFormat[BigInt] {
    def write(i: BigInt) = JsString(i.toString(16))

    def read(i: JsValue) = i match {
      case JsString(num) => BigInt(num, 16)
      case _ => deserializationError("bigint string expected")
    }
  }

  implicit object SecretKeyJson extends RootJsonFormat[SecretKey] {
    def write(k: SecretKey) = BigInt(k.getEncoded).toJson
    def read (k: JsValue) = k match {
      case JsString(num) =>
        val byteArray = BigInt(num, 16).toByteArray
        new SecretKeySpec(byteArray, 0, byteArray.length, "AES")
      case _ => deserializationError("hex string SecretKey expected")
    }
  }

  implicit val userFormat = jsonFormat11(F_User.apply)
  implicit val userEFormat = jsonFormat11(F_UserE.apply)
  implicit val profileFormat = jsonFormat8(F_UserProfile.apply)
  implicit val profileEFormat = jsonFormat8(F_UserProfileE.apply)
  implicit val postFormat = jsonFormat6(F_Post.apply)
  implicit val postEFormat = jsonFormat6(F_PostE.apply)
  implicit val pictureFormat = jsonFormat7(F_Picture.apply)
  implicit val pictureEFormat = jsonFormat7(F_PictureE.apply)
  implicit val pictureTransmitFormat = jsonFormat2(F_PictureTransmit.apply)
  implicit val albumFormat = jsonFormat7(F_Album.apply)
  implicit val albumEFormat = jsonFormat7(F_AlbumE.apply)
  implicit val pageFormat = jsonFormat10(F_Page.apply)
}
