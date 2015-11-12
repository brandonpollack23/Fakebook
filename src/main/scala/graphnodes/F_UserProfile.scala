package graphnodes

import java.util.Date

import akka.actor.Actor

case class ProfileID(id: BigInt)

case class F_UserProfile(posts: List[F_Post],
                       dateOfCreation: Date,
                       userList: List[UserID],
                       profilePicture: UserID,
                       bio: String,
                       friendList: List[F_User],
                       user: UserID,
                       id: ProfileID)

class F_ProfileHandler(id: ProfileID) extends Actor {
  import system.profiles

  val user = profiles.get(id)

  def receive = {
    case Post(p) =>
      profiles.put(id, user.copy(posts = p :: user.posts))

    case RemovePost(id: PostID) =>
      profiles.put(id, user.copy(posts = user.posts.filter(_ != id)))
  }

  case class Post(p: F_Post)
  case class RemovePost(id: PostID)
}
