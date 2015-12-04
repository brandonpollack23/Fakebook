package system.workers

import java.io.{FileOutputStream, FileInputStream, File}
import java.util.{Date, MissingFormatArgumentException}

import akka.actor
import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import akka.pattern.{pipe, ask}
import akka.util.Timeout
import graphnodes._
import spray.http.{Uri, HttpRequest}
import system.F_BackBone._
import util.MyJsonProtocol

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import spray.json._
import MyJsonProtocol._

import language.postfixOps

class F_PictureHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import F_PictureHandler._
  import context.dispatcher

  implicit val timeout = Timeout(5 seconds)

  val albums = collection.mutable.Map[BigInt, F_AlbumE]()
  val pictures = collection.mutable.Map[BigInt, F_PictureE]()
  val pictureData = collection.mutable.Map[BigInt, File]()

  val defaultPictureFile = new File("images/defaultpic.jpg")

  pictureData.put(defaultPictureDataID, defaultPictureFile)

  def receive = {
    case GetPictureInfo(id) =>
      val replyTo = sender()

      pictures.get(id) match {
        case Some(pic) => Future(pic.toJson.compactPrint) pipeTo replyTo
        case None => replyTo ! noSuchPictureFailure(id)
      }

    case GetAlbumInfo(id) =>
      val replyTo = sender()

      albums.get(id) match {
        case Some(alb) => Future(alb.toJson.compactPrint) pipeTo replyTo
        case None => replyTo ! noSuchAlbumFailure(id)
      }

    case GetImage(id) => //does not send back JSON, sends image
      pictureData.get(id) match {
        case Some(image) =>
          val byteBuffer = new Array[Byte](image.length.asInstanceOf[Int]) //TODO UPDATE TO STREAMING SOCKET, this will crash the server as is if files are large
          new FileInputStream(image).read(byteBuffer)
          sender ! byteBuffer
        case None => sender ! noSuchImageFailure(id)
      }

    case PutImage(request) =>
      createImage(request)

    case CreateAlbum(request) =>
      createAlbum(request)

    case CreateDefaultAlbum(ownerID) =>
      val id = getUniqueRandomBigInt(albums)
      albums.put(id, F_AlbumE("Default Album".getBytes, "default album generated for you by Fakebook".getBytes, new Date, isDefault =  true, ownerID, id, List[BigInt]()))
      sender ! id

    case UpdateImageData(id, request) =>
      updatePicture(id, request)

    case UpdateAlbumData(id, request) =>
      updateAlbum(id, request)

    case DeletePicture(id) =>
      deletePicture(id)

    case DeleteAlbum(id, over) =>
      deleteAlbum(id, over)
  }

  def updatePicture(id: BigInt, request: HttpRequest) = {
    def updateCurrentPicture(picture: F_PictureE, fields: Map[String, JsValue]): F_PictureE = {
      if(fields.isEmpty) picture
      else {
        val currentField = fields.head
        currentField._1 match {
          case F_Picture.`nameField` => updateCurrentPicture(picture.copy(name = currentField._2.convertTo[Array[Byte]]), fields.tail)
          case F_Picture.`descriptionField` => updateCurrentPicture(picture.copy(description = currentField._2.convertTo[Array[Byte]]), fields.tail)
          case F_Picture.`albumField` =>
            val newAlbumID = BigInt(currentField._2.toString(), 16)
            val oldAlbumID = picture.containingAlbum
            val oldAlbum = albums.get(oldAlbum).get
            albums.get(newAlbumID) match {
              case Some(alb) => albums.put(newAlbumID, alb.copy(images = picture.pictureID :: alb.images)) //put it in the new album
              case None =>
                throw noSuchAlbumException(List(newAlbumID)) //that isn't an album
            }
            albums.put(oldAlbumID, oldAlbum.copy(images = oldAlbum.images.filter(_ != picture.pictureID))) //remove from old album
            updateCurrentPicture(picture.copy(containingAlbum = newAlbumID), fields.tail)
          case _ => updateCurrentPicture(picture, fields.tail)
        }
      }
    }

    try{
      val picture = pictures.getOrElse(id, throw noSuchPictureException(List(id)))

      val fields = request.entity.asString.parseJson.asJsObject.fields

      val updatedPicture = updateCurrentPicture(picture, fields)

      pictures.put(id, updatedPicture)

      val replyTo = sender()

      Future(updatedPicture.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updateAlbum(id: BigInt, request: HttpRequest) = { //TODO continue from here
    def updateCurrentAlbum(album: F_AlbumE, fields: Map[String, JsValue]): F_AlbumE = {
      if(fields.isEmpty) album
      else {
        val currentField = fields.head
        currentField._1 match {
          case F_Album.`nameField` => updateCurrentAlbum(album.copy(name = currentField._2.convertTo[Array[Byte]]),fields.tail)
          case F_Picture.`descriptionField` => updateCurrentAlbum(album.copy(description = currentField._2.convertTo[Array[Byte]]), fields.tail)
          case _ => updateCurrentAlbum(album, fields.tail)
        }
      }
    }

    try{
      val album = albums.getOrElse(id, throw noSuchAlbumException(List(id)))

      val fields = request.entity.asString.parseJson.asJsObject.fields

      val updatedAlbum = updateCurrentAlbum(album, fields)

      albums.put(id, updatedAlbum)

      val replyTo = sender()

      Future(updatedAlbum.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def createImage(request: HttpRequest) {
    val pictureID = getUniqueRandomBigInt(pictures)
    val imageID = placeImage(request)

    try {
      val newPicture = request.entity.asString.parseJson.convertTo[F_PictureE].copy(fileID = imageID, pictureID = pictureID, dateOfCreation = new Date)
      val albumID = newPicture.containingAlbum
      albums.get(albumID) match {
        case Some(album) =>
          albums.put(albumID, album.copy(images = newPicture.pictureID :: album.images))
        case None =>
          throw noSuchAlbumException(List(albumID))
      }

      pictures.put(pictureID, newPicture)

      val replyTo = sender()

      Future(newPicture.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender() ! actor.Status.Failure(ex)
    }
  }

  def createAlbum(request: HttpRequest) {
    val albumID = getUniqueRandomBigInt(albums)

    try {
      val newAlbum = request.entity.asString.parseJson.convertTo[F_AlbumE].copy(id = albumID, dateOfCreation = new Date, images = List[BigInt]())
      albums.put(newAlbum.id, newAlbum)
      val replyTo = sender()
      Future(newAlbum.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender() ! actor.Status.Failure(ex)
    }
  }

  def deletePicture(id: BigInt) = {
    try {
      pictures.remove(id) match {
        case Some(pic) =>
          val album = albums.getOrElse(pic.containingAlbum, throw noSuchAlbumException(List(pic.containingAlbum)))
          albums.put(pic.containingAlbum, album.copy(images = album.images.filter(_ != id)))
          removePictureData(pic.fileID)
          sender ! "Picture Deleted!"
        case None => sender ! noSuchPictureFailure(id)
      }
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def removePictureData(id: BigInt) = {
    pictureData.remove(id) match {
      case Some(x) =>
        x.delete
      case None =>
        throw noSuchImageException(List(id))
    }
  }

  def deleteAlbum(id: BigInt, defaultOverride: Boolean = false) = {
    val album = albums.getOrElse(id, throw noSuchAlbumException(List(id)))
    if(album.isDefault && !defaultOverride) sender ! "Cannot delete default albums"
    else {
      albums.remove(id) match {
        case Some(albumx) =>
          albumx.images.foreach(deletePicture)
          sender ! "Album Deleted!"
        case None => sender ! noSuchAlbumFailure(id)
      }
    }
  }

  def placeImage(request: HttpRequest) = { //places image in database and returns the id
    val imageID = getUniqueRandomBigInt(pictureData)
    //SECURITY CHANGE make sure it is actually an image (not now)
    val file = new File("./images/" + imageID + ".jpg")
    new FileOutputStream(file).write(request.entity.data.toByteArray)
    pictureData.put(imageID, file)
    imageID
  }
}

object F_PictureHandler {
  def props(backbone: ActorRef) = Props(new F_PictureHandler(backbone))

  val defaultPictureID = BigInt(0)
  val defaultPictureDataID = BigInt(0)

  /**
   * exception to throw or put in messages when no such picture
   * @param id id
   * @return exception
   */
  private def noSuchPictureException(id: Seq[BigInt]) = new NoSuchElementException("there is no picture entry for " + id.mkString(", "))

  /**
   * message to send when picture id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchPictureFailure(ids: BigInt*) = actor.Status.Failure(noSuchPictureException(ids))

  /**
   * exception to throw or put in messages when no such image
   * @param id id
   * @return exception
   */
  private def noSuchImageException(id: Seq[BigInt]) = new NoSuchElementException("there is no image entry for " + id.mkString(", "))

  /**
   * message to send when image id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchImageFailure(ids: BigInt*) = actor.Status.Failure(noSuchImageException(ids))

  /**
   * exception to throw or put in messages when no such album
   * @param id id
   * @return exception
   */
  private def noSuchAlbumException(id: Seq[BigInt]) = new NoSuchElementException("there is no album entry for " + id.mkString(", "))

  /**
   * message to send when album id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchAlbumFailure(ids: BigInt*) = actor.Status.Failure(noSuchAlbumException(ids))
}