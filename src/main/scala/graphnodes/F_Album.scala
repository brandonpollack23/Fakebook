package graphnodes

import java.security.Key
import java.util.Date
import util.Crypto._

case class F_Album(name: String, description: String,
                    dateOfCreation: Date,
                    isDefault: Boolean,
                    ownerID: BigInt,
                    id: BigInt,
                    images: List[BigInt]) {
  def encryptAlbum(key: Key) = {
    F_AlbumE(name.getBytes.encryptAES(key), description.getBytes.encryptAES(key), dateOfCreation, isDefault,
     ownerID, id, images)
  }
}

case class F_AlbumE(name: Array[Byte], description: Array[Byte],
                   dateOfCreation: Date,
                   isDefault: Boolean,
                   ownerID: BigInt,
                   id: BigInt,
                   images: List[BigInt]) {
  def decryptAlbumE(key: Key) = {
    F_Album(name.decryptAES(key).byteArray2String, description.decryptAES(key).byteArray2String,
      dateOfCreation, isDefault, ownerID, id, images)
  }
}

object F_Album {
  //changable stuff
  val nameField = "name"
  val descriptionField = "description"
}