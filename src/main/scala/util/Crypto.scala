package util

import java.io.{ObjectInputStream, ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}
import java.nio.ByteBuffer
import java.security.{Key, PrivateKey, PublicKey}
import javax.crypto.Cipher

import language.implicitConversions

object Crypto {
  implicit class ByteArrayIntConverter(x: Int) {
    def toByteArray: Array[Byte] = ByteBuffer.allocate(4).putInt(x).array()
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