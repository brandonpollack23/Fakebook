package system.workers

import java.io.File

import akka.actor
import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import graphnodes.{F_Picture, F_Album}
import system.F_BackBone._

//TODO implement the logic for each transaction
class F_PictureHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import F_PictureHandler._

  val albums = collection.mutable.Map[BigInt, F_Album]()
  val pictures = collection.mutable.Map[BigInt, F_Picture]()
  val pictureData = collection.mutable.Map[BigInt, File]()

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