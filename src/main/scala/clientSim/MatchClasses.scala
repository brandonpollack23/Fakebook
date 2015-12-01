package clientSim

import java.util.Date
import graphnodes._

object MatchClasses {

  case class Test1()
  case class Test2()
  case class Test3()

case class Begin()

case class Simulate()

case class PerformAction()

case class createUser(fname: String, lName: String, bio: String, age: Int, dob: Date)

case class createPost(posterId: BigInt, content: String, locationType: String, locationId: BigInt)

case class createPage(userId: BigInt, pName: String, pDes: String)

case class createAlbum(id: BigInt, albmName: String, albmDes: String)

case class uploadPicture(pName: String, pDes: String, albumID: BigInt, userId: BigInt)

case class updateUserData(userID: BigInt, fName: String, lName: String, bio: String)

case class updateUserProfile(profId: BigInt, des: String)

case class updatePictureData(pName: String, pDes: String, albumID: BigInt, userId: BigInt)

case class updateAlbumData(useId: BigInt, albmId: BigInt, albmName: String, albmDes: String)

case class updatePost(id: BigInt, postID: BigInt, locationType: String, contents: String)

case class updatePageData(userId: BigInt, pageID:BigInt , pName: String, pDes: String)

case class getUserData(id: BigInt)

case class getUserProfile(id: BigInt)

case class getPictureData(userId: BigInt, picID: BigInt)

case class getAlbumData(userId: BigInt, albmId: BigInt)

case class getPost(userId: BigInt, postId: BigInt)

case class getPageData(id: BigInt)

case class deleteUser(id: BigInt)

case class deletePicture(userId: BigInt, picId: BigInt)

case class deleteAlbum(userId: BigInt, albmId: BigInt)

case class deletePost(id: BigInt, postId: BigInt)

case class deletePage(id: BigInt)

case class sendFriendReq(userId: BigInt, frndId: BigInt)

case class acceptFriendReq(userId: BigInt, frndId: BigInt)

case class userCreated(res: F_User)

case class postCreated(res: F_Post, locationType: String)

case class pageCreated(res: F_Page)

case class pictureUploaded(res: F_Picture)

case class albumCreated(res: F_Album)

case class profileCreated(res: F_UserProfile)

case class userEdited(res: F_User)

case class userProfileEdited(res: F_UserProfile)

case class postEdited(res: F_Post)

case class pageEdited(res: F_Page)

case class pictureEdited(res: F_Picture)

case class albumEdited(res: F_Album)

case class profileEdited(res: F_UserProfile)

case class userRetrieved(res: F_User)

case class userProfileRetrieved(res: F_UserProfile)

case class postRetrieved(res: F_Post)

case class pageRetrieved(res: F_Page)

case class pictureRetrieved(res: F_Picture)

case class albumRetrieved(res: F_Album)

case class profileRetrieved(res: F_UserProfile)

case class userDeleted(res: String)

case class postDeleted(res: F_Post)

case class pageDeleted(res: F_Page)

case class pictureDeleted(res: String)

case class albumDeleted(res: String)

case class profileDeleted(res: String)

case class friendRequestSent()

case class friendRequestReceived()

case class friendRequestAccepted()

case class friendDeleted()

}