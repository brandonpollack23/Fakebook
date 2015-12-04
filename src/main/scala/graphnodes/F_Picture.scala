package graphnodes

import java.security.Key
import java.util.Date
import util.Crypto._

case class F_Picture(name: String, description: String,
                      containingAlbum: BigInt,
                      dateOfCreation: Date,
                      fileID: BigInt,
                      pictureID: BigInt,
                      ownerID: BigInt) {
  def encryptPicture(key: Key) = {
    F_PictureE(name.getBytes.encryptAES(key), description.getBytes.encryptAES(key), containingAlbum,
      dateOfCreation, fileID, pictureID, ownerID)
  }
}

case class F_PictureE(name: Array[Byte], description: Array[Byte],
                     containingAlbum: BigInt,
                     dateOfCreation: Date,
                     fileID: BigInt,
                     pictureID: BigInt,
                     ownerID: BigInt) {
  def decryptPictureE(key: Key) = {
    F_Picture(name.decryptAES(key).byteArray2String, description.decryptAES(key).byteArray2String,
      containingAlbum, dateOfCreation,
      fileID, pictureID, ownerID)
  }
}

case class F_PictureTransmit(pictureInfo: F_PictureE, picture: Array[Byte])

object F_Picture {
  //changable
  val nameField = "name"
  val descriptionField = "description"
  val albumField = "containingAlbum" //albumID to move to is the value of this query
}
