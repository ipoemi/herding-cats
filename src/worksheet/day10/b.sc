case class User(id: Long, name: String)

sealed trait Error

object Error {

  sealed case class UserNotFound(userId: Long) extends Error

  sealed case class ConnectionError(message: String) extends Error

}

object UserRepo {
  def followers(userId: Long): Either[Error, List[User]] = ???
}

def isFriends0(user1: Long, user2: Long): Either[Error, Boolean] =
  for {
    a <- UserRepo.followers(user1).right
    b <- UserRepo.followers(user2).right
  } yield a.exists(_.id == user2) && b.exists(_.id == user1)


import scala.concurrent.{Future, ExecutionContext}

object UserRepo1 {
  def followers(userId: Long): Future[Either[Error, List[User]]] = ???
}

def isFriends1(user1: Long, user2: Long)(implicit ec: ExecutionContext): Future[Either[Error, Boolean]] =
  for {
    a <- UserRepo1.followers(user1)
    b <- UserRepo1.followers(user2)
  } yield for {
    x <- a.right
    y <- b.right
  } yield x.exists(_.id == user2) && y.exists(_.id == user1)


def isFriends2(user1: Long, user2: Long)(implicit ec: ExecutionContext): Future[Either[Error, Boolean]] =
  UserRepo1.followers(user1) flatMap {
    case Right(a) => UserRepo1.followers(user2) map {
      case Right(b) =>
        Right(a.exists(_.id == user2) && b.exists(_.id == user1))
      case Left(e) => Left(e)
    }
    case Left(e) => Future.successful(Left(e))
  }


import cats._, cats.data._, cats.implicits._

object UserRepo2 {
  def followers(userId: Long)(implicit ec: ExecutionContext): EitherT[Future, Error, List[User]] =
    userId match {
      case 0L =>
        EitherT.right(Future {
          List(User(1, "Michael"))
        })
      case 1L =>
        EitherT.right(Future {
          List(User(0, "Vito"))
        })
      case x =>
        println("not found")
        EitherT.left(Future.successful {
          Error.UserNotFound(x)
        })
    }
}

def isFriends3(user1: Long, user2: Long)
              (implicit ec: ExecutionContext): EitherT[Future, Error, Boolean] =
  for {
    a <- UserRepo2.followers(user1)
    b <- UserRepo2.followers(user2)
  } yield a.exists(_.id == user2) && b.exists(_.id == user1)


implicit val ec = scala.concurrent.ExecutionContext.global

import scala.concurrent.Await

import scala.concurrent.duration._

Await.result(isFriends3(0, 1).value, 1 second)

Await.result(isFriends3(2, 3).value, 1 second)


