package system.workers

import java.io.File
import java.util.{Date, MissingFormatArgumentException}

import akka.actor
import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import graphnodes.{F_Picture, F_Album}
import spray.http.{Uri, HttpRequest}
import system.F_BackBone._

//TODO implement the logic for each transaction
class F_PictureHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import F_PictureHandler._

  val albums = collection.mutable.Map[BigInt, F_Album]()
  val pictures = collection.mutable.Map[BigInt, F_Picture]()
  val pictureData = collection.mutable.Map[BigInt, Array[Byte]]()

  def receive = {
    case GetPictureInfo(id) =>
      pictures.get(id) match {
        case Some(pic) => sender ! pic //TODO change to JSON
        case None => sender ! noSuchPictureFailure(id)
      }

    case GetAlbumInfo(id) =>
      albums.get(id) match {
        case Some(alb) => sender ! alb //TODO change to JSON
        case None => sender ! noSuchAlbumFailure(id)
      }

    case GetImage(id) => //does not send back JSON, sends image
      pictureData.get(id) match {
        case Some(image) => sender ! image
        case None => sender ! noSuchImageFailure(id)
      }

    case PutImage(request) =>
      createImage(request)

    case CreateAlbum(request) =>
      createAlbum(request)

    case UpdateImageData(id, request) =>
      updatePicture(id, request)

    case UpdateAlbumData(id, request) =>
      updateAlbum(id, request)

    case DeletePicture(id) =>
      deletePicture(id)

    case DeleteAlbum(id) =>
      deleteAlbum(id)
  }

  def updatePicture(id: BigInt, request: HttpRequest) = ???

  def updateAlbum(id: BigInt, request: HttpRequest) = ???

  def createImage(request: HttpRequest) = ???

  def createAlbum(request: HttpRequest) = ???

  def deletePicture(id: BigInt) = ???

  def deleteAlbum(id: BigInt) = ???

  def placeImage(request: HttpRequest) = { //places image in database and returns the id
    val imageID = getUniqueRandomBigInt(pictureData)
    //SECURITY CHANGE make sure it is actually an image (not now)
    pictureData.put(imageID, request.entity.data.toByteArray)
    imageID
  }
}

object F_PictureHandler {
  def props(backbone: ActorRef) = Props(new F_PictureHandler(backbone))

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