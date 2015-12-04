package util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.ByteBuffer
import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, Key}
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import scala.language.implicitConversions

object Crypto {
  implicit class ByteArrayIntConverter(x: Int) {
    def toByteArray: Array[Byte] = ByteBuffer.allocate(4).putInt(x).array()
  }

  implicit class ByteArrayBoolConverter(x: Boolean) {
    def toByteArray: Array[Byte] = if(x) new Array(1) else new Array(0)
  }

  implicit class ByteArrayToSecretKey(x: Array[Byte]) {
    def toSecretKey = new SecretKeySpec(x, 0, x.length, "AES")
  }

  implicit class ByteArrayToRSAPublic(x: Array[Byte]) {
    def toPublicKey = KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(x))
  }

  implicit class ByteArrayToRSAPrivate(x: Array[Byte]) {
    def toPrivateKey = KeyFactory.getInstance("RSA").generatePrivate(new X509EncodedKeySpec(x))
  }

  implicit class ByteArrayAnyConverter(x: java.io.Serializable) {
    def toByteArray: Array[Byte] = {
      val b = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(b)
      oos.writeObject(x)
      b.toByteArray
    }
  }

  implicit class Deserializer(x: Array[Byte]) {
    def streamDeserializeToObject[T]: T = {
      val b = new ByteArrayInputStream(x)
      val o = new ObjectInputStream(b)
      o.readObject().asInstanceOf[T]
    }

    def byteArray2Int: Int = ByteBuffer.wrap(x).getInt
    def byteArray2String: String = new String(x)
    def byteArray2Bool: Boolean = x(0) == 1
  }

  implicit class Encrypter(value: Array[Byte]) {
    def encryptRSA(key: Key) = {
      val cipher = Cipher.getInstance("RSA")
      cipher.init(Cipher.ENCRYPT_MODE, key)
      cipher.doFinal(value)
    }

    def encryptAES(key: Key) = {
      val cipher = Cipher.getInstance("AES")
      cipher.init(Cipher.ENCRYPT_MODE, key)
      cipher.doFinal(value)
    }
  }

  implicit class Decrypter(value: Array[Byte]) {
    def decryptRSA(key: Key) = {
      val cipher = Cipher.getInstance("RSA")
      cipher.init(Cipher.DECRYPT_MODE, key)
      cipher.doFinal(value)
    }

    def decryptAES(key: Key) = {
      val cipher = Cipher.getInstance("AES")
      cipher.init(Cipher.DECRYPT_MODE, key)
      cipher.doFinal(value)
    }
  }
}