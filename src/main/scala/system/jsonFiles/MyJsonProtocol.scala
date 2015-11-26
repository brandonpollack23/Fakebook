package system.jsonFiles

import java.util.Date
import spray.json._
import graphnodes._
//import DefaultJsonProtocol._

object MyJsonProtocol extends DefaultJsonProtocol {


  //protocol for F_Album object
  implicit object AlbumJsonFormat extends RootJsonFormat[F_Album] {

    def write(p: F_Album) =
                          JsArray(JsString(p.name),
                          JsString(p.description),
                          JsNumber(p.dateOfCreation.getTime),
                          JsString(p.ownerID.toString(16)),
                          JsString(p.id.toString(16)),
                          JsArray(p.images.map(_.toJson).toVector) )

    def read(value: JsValue) = value match {
                    case  JsArray(Vector(JsString(name),
                          JsString(description),
                          JsNumber(date),
                          JsString(ownerId),
                          JsString(id),
                          JsArray(images))) =>
                      new F_Album(name,
                          description,
                          new Date(date.toLong),
                          BigInt(ownerId),
                          BigInt(id),
                          images.map(_.convertTo[BigInt])(collection.breakOut) )
                    case _ => deserializationError("Album Expected")
    }
  }

  //protocol for F_Page object
  implicit object PageJsonFormat extends RootJsonFormat[F_Page] {
    def write(p: F_Page) =
                          JsArray(JsString(p.name),
                          JsString(p.description),
                          JsNumber(p.dateOfCreation.getTime),
                          JsArray(p.userList.map(_.toJson).toVector),
                          JsArray(p.posts.map(_.toJson).toVector),
                          JsArray(p.albumIDs.map(_.toJson).toVector),
                          JsString(p.pictureID.toString(16)),
                          JsString(p.ownerID.toString(16)) )

    def read(value: JsValue) = value match {
                        case JsArray(Vector(JsString(name),
                             JsString(description),
                             JsNumber(dateOfCreation),
                             JsArray(userList),
                             JsArray(posts),
                             JsArray(albumIDs),
                             JsString(pictureID),
                             JsString(ownerID) )) =>
                         new F_Page(name,
                             description,
                             new java.util.Date(dateOfCreation.toLong),
                             userList.map(_.convertTo[BigInt])(collection.breakOut),
                             posts.map(_.convertTo[BigInt])(collection.breakOut),
                             albumIDs.map(_.convertTo[BigInt])(collection.breakOut),
                             BigInt(pictureID),
                             BigInt(ownerID) )
                    case _ => deserializationError("Page Expected")
    }
  }

  //protocol for F_Picture object
  implicit object PictureJsonFormat extends RootJsonFormat[F_Picture] {
    def write(p: F_Picture) =
                            JsArray(JsString(p.name),
                            JsString(p.description),
                            JsNumber(p.dateOfCreation.getTime),
                            JsString(p.fileID.toString(16)),
                            JsString(p.ownerID.toString(16)) )

    def read(value: JsValue) = value match {
                       case JsArray(Vector(JsString(name),
                            JsString(description),
                            JsNumber(date),
                            JsString(fileID),
                            JsString(ownerID))) =>
                        new F_Picture(name,
                            description,
                            new java.util.Date(date.toLong),
                            BigInt(fileID),
                            BigInt(ownerID))
                       case _ => deserializationError("Picture Expected")
    }
  }


  //protocol for F_Post object
  implicit object PostJsonFormat extends RootJsonFormat[F_Post] {
    def write(p: F_Post) =
                          JsArray(JsString(p.contents),
                          JsString(p.creator.toString(16)),
                          JsString(p.locationType),
                          JsString(p.location.toString(16)),
                          JsNumber(p.dateOfCreation.getTime),
                          JsString(p.postID.toString(16)) )

    def read(value: JsValue) = value match {
                     case JsArray(Vector(JsString(contents),
                          JsString(creator),
                          JsString(locationType),
                          JsString(location),
                          JsNumber(dateOfCreation),
                          JsString(postID))) =>
                      new F_Post(contents,
                          BigInt(creator),
                          locationType,
                          BigInt(location),
                          new Date(dateOfCreation.toLong),
                          BigInt(postID) )
                     case _ => deserializationError("Post Expected")
    }
  }

  //protocol for F_User object
  implicit object UserJsonFormat extends RootJsonFormat[F_User] {
    def write(p: F_User) =
                          JsArray(JsString(p.firstName),
                          JsString(p.lastName),
                          JsString(p.biography),
                          JsNumber(p.age),
                          JsNumber(p.dateOfBirth.getTime),
                          JsNumber(p.dateOfCreation.getTime),
                          JsArray(p.friends.map(_.toJson).toVector),
                          JsArray(p.friendRequests.map(_.toJson).toVector),
                          JsString(p.profileID.toString(16)),
                          JsString(p.userID.toString(16)) )

    def read(value: JsValue) = value match {
                               case JsArray(Vector(JsString(firstName),
                                    JsString(lastName),
                                    JsString(biography),
                                    JsNumber(age),
                                    JsString(dateOfBirth),
                                    JsString(dateOfCreation),
                                    JsArray(friends),
                                    JsArray(friendRequests),
                                    JsString(profileID),
                                    JsString(userID))) =>
                                new F_User(firstName,
                                    lastName,
                                    biography,
                                    age.toInt,
                                    new Date(dateOfBirth.toLong),
                                    new Date(dateOfCreation.toLong),
                                    friends.map(_.convertTo[BigInt])(collection.breakOut),
                                    friendRequests.map(_.convertTo[BigInt])(collection.breakOut),
                                    BigInt(profileID),
                                    BigInt(userID))
                               case _ => deserializationError("User Expected")
    }
  }

  //protocol for F_UserProfile object
  implicit object UserProfileJsonFormat extends RootJsonFormat[F_UserProfile] {
    def write(p: F_UserProfile) =
                                  JsArray(JsArray(p.posts.map(_.toJson).toVector),
                                  JsNumber(p.dateOfCreation.getTime),
                                  JsArray(p.albumIDs.map(_.toJson).toVector),
                                  JsString(p.profilePictureID.toString(16)),
                                  JsString(p.description),
                                  JsString(p.profileID.toString(16)))

    def read(value: JsValue) = value match {
                              case JsArray(Vector(JsArray(posts),
                                   JsNumber(dateOfCreation),
                                   JsArray(albumIDs),
                                   JsString(profilePictureID),
                                   JsString(description),
                                   JsString(profileID))) =>
                               new F_UserProfile(posts.map(_.convertTo[BigInt])(collection.breakOut),
                                   new Date(dateOfCreation.toLong),
                                   albumIDs.map(_.convertTo[BigInt])(collection.breakOut),
                                   BigInt(profilePictureID),
                                   description,
                                   BigInt(profileID))
                              case _ => deserializationError("User_Profile Expected")
    }
  }

}

//Alternate way to get own protocol which didn't work
/*
object MyJsonProtocol1 extends DefaultJsonProtocol {

  implicit val userFormat    = jsonFormat10(F_User.apply)
  implicit val profileFormat = jsonFormat6(F_UserProfile)
  implicit val postFormat    = jsonFormat6(F_Post.apply)
  implicit val pictureFormat = jsonFormat5(F_Picture.apply)
  implicit val albumFormat   = jsonFormat6(F_Album.apply)
  implicit val pageFormat    = jsonFormat8(F_Page.apply)

}*/