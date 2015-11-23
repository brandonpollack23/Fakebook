package clientSim

import spray.http.{Uri, HttpRequest, HttpResponse}
import spray.http._
import HttpMethods._
import akka.pattern.ask
import akka.actor.Actor
import spray.can.Http
import graphnodes._
import akka.io.IO
import spray.json._
import akka.actor._
import DefaultJsonProtocol._
import java.util._
import java.net.InetSocketAddress._
import scala.concurrent.Future
import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats
import spray.client.pipelining._


class F_BaseClient extends Actor with SprayJsonSupport with AdditionalFormats{

def receive = {

  //Create operations
  case createUser => {

  }

  case createPost => {

  }

  case createPage => {

  }

  case createAlbum => {

  }

  case uploadPicture => {

  }

  //Update operations
  case updateUserData => {

  }

  case updatePictureData => {

  }


  case updateAlbumData => {

  }


  case updatePost => {

  }

  case updatePageData => {

  }

  //get operations
  case getUserData => {

  }

  case getImageData => {

  }

  case getAlbumData => {

  }

  case getPost => {

  }

  case getPageData => {

  }

  //Delete operations
  case deleteUser => {

  }

  case deleteImage => {

  }

  case deleteAlbum => {

  }

  case deletePost => {

  }

  case deletePage => {

  }

  //Friend operations
  case sendFriendReq => {

  }

  case acceptFriendReq => {

  }



}


}
