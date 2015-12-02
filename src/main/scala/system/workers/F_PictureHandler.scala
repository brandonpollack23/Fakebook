package system.workers

import java.io.{FileOutputStream, FileInputStream, File}
import java.util.{Date, MissingFormatArgumentException}

import akka.actor
import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import akka.pattern.{pipe, ask}
import akka.util.Timeout
import graphnodes.{F_UserProfile, F_User, F_Picture, F_Album}
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

  val albums = collection.mutable.Map[BigInt, F_Album]()
  val pictures = collection.mutable.Map[BigInt, F_Picture]()
  val pictureData = collection.mutable.Map[BigInt, File]()

  val defaultPictureFile = new File("images/defaultpic.jpg")

  pictureData.put(defaultPictureDataID, defaultPictureFile)

  def receive = {
    case GetPictureInfo(id) =>
      val replyTo = sender

      pictures.get(id) match {
        case Some(pic) => Future(pic.toJson.compactPrint) pipeTo replyTo
        case None => replyTo ! noSuchPictureFailure(id)
      }

    case GetAlbumInfo(id) =>
      val replyTo = sender

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
      albums.put(id, F_Album("Default Album", "default album generated for you by Fakebook", new Date, isDefault = true, ownerID, id, List[BigInt]()))
      sender ! id

    case UpdateImageData(id, request) =>
      updatePicture(id, request)

    case UpdateAlbumData(id, request) =>
      updateAlbum(id, request)

    case DeletePicture(id) =>
      deletePicture(id)

    case DeleteAlbum(id) =>
      deleteAlbum(id)
  }

  def updatePicture(id: BigInt, request: HttpRequest) = {
    def updateCurrentUserInstance(picture: F_Picture, params: Uri.Query, parametersRemaining: List[String]): F_Picture = {
      if(parametersRemaining.isEmpty) {
        picture
      } else {
        val currentParameter = parametersRemaining.head
        params.get(currentParameter) match {
          case Some(value) =>
            currentParameter match {
              case F_Picture.`nameString` => updateCurrentUserInstance(picture.copy(name = value), params, parametersRemaining.tail)
              case F_Picture.`descriptionString` => updateCurrentUserInstance(picture.copy(description = value), params, parametersRemaining.tail)
              case F_Picture.`albumString` => updateCurrentUserInstance(picture.copy(containingAlbum = BigInt(value, 16)), params, parametersRemaining.tail)
              case _ => throw new IllegalArgumentException("there is no case to handle such parameter in the list (system issue)")
            }
          case None =>
            updateCurrentUserInstance(picture, params, parametersRemaining.tail)
        }
      }
    }

    try{
      val user = pictures.getOrElse(id, throw noSuchPictureException(List(id)))

      val params = request.uri.query

      val updatedPicture = updateCurrentUserInstance(user, params, F_Picture.changableParameters)

      pictures.put(id, updatedPicture)

      val replyTo = sender

      Future(updatedPicture.toJson.compactPrint) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updateAlbum(id: BigInt, request: HttpRequest) = {
    def updateCurrentUserInstance(album: F_Album, params: Uri.Query, parametersRemaining: List[String]): F_Album = {
      if(parametersRemaining.isEmpty) {
        album
      } else {
        val currentParameter = parametersRemaining.head
        params.get(currentParameter) match {
          case Some(value) =>
            currentParameter match {
              case F_Picture.`nameString` => updateCurrentUserInstance(album.copy(name = value), params, parametersRemaining.tail)
              case F_Picture.`descriptionString` => updateCurrentUserInstance(album.copy(description = value), params, parametersRemaining.tail)
              case _ => throw new IllegalArgumentException("there is no case to handle such parameter in the list (system issue)")
            }
          case None =>
            updateCurrentUserInstance(album, params, parametersRemaining.tail)
        }
      }
    }

    try{
      val user = albums.getOrElse(id, throw noSuchAlbumException(List(id)))

      val params = request.uri.query

      val updatedAlbum = updateCurrentUserInstance(user, params, F_Picture.changableParameters)

      albums.put(id, updatedAlbum)

      val replyTo = sender

      Future(updatedAlbum.toJson.compactPrint) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def createImage(request: HttpRequest) = {
    val pictureID = getUniqueRandomBigInt(pictures)
    val imageID = placeImage(request)
    val params = request.uri.query

    def getAllComponents = {
      def extractComponent(key: String) = {
        params.find(_._1 == key).map(_._2) match {
          case Some(x) => x
          case None => throw new MissingFormatArgumentException("there is no entry for " + key)
        }
      }

      val ownerID = BigInt(extractComponent(F_Picture.ownerString), 16)

      val albumID = params.find(_._1 == F_Picture.albumString) match {
        case Some(x) =>
          val tempAlbumID = BigInt(x._2, 16)
          if(albums.contains(tempAlbumID)) tempAlbumID else throw noSuchAlbumException(List(tempAlbumID))
        case None =>
          Await.result((backbone ? GetUserInfo(ownerID)).mapTo[F_User].map(x => backbone ? GetProfileInfo(x.profileID)).mapTo[F_UserProfile].map(_.albumIDs.last), 5 seconds)
      }

      (extractComponent(F_Picture.nameString), extractComponent(F_Picture.descriptionString), albumID,  new Date, imageID, pictureID, ownerID)
    }

    try {
      val newPicture = (F_Picture.apply _).tupled(getAllComponents)
      pictures.put(pictureID, newPicture)
      val replyTo = sender
      Future(newPicture.toJson.compactPrint) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def createAlbum(request: HttpRequest) = {
    val albumID = getUniqueRandomBigInt(pictures)
    val params = request.uri.query

    def getAllComponents = {
      def extractComponent(key: String) = {
        params.find(_._1 == key).map(_._2) match {
          case Some(x) => x
          case None => throw new MissingFormatArgumentException("there is no entry for " + key)
        }
      }

      (extractComponent(F_Album.nameString), extractComponent(F_Album.descriptionString), new Date, false, BigInt(extractComponent(F_Album.ownerString), 16), albumID, List[BigInt]())
    }

    try {
      val newAlbum = (F_Album.apply _).tupled(getAllComponents)
      albums.put(albumID, newAlbum)
      val replyTo = sender
      Future(newAlbum.toJson.compactPrint) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
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

  def deleteAlbum(id: BigInt) = {
    val album = albums.getOrElse(id, throw noSuchAlbumException(List(id)))
    if(album.isDefault) sender ! "Cannot delete default albums"
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