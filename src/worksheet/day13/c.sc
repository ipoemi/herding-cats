import cats._, cats.data._, cats.implicits._

case class User(id: Long, name: String)

sealed trait Error

object Error {

  final case class UserNotFound(userId: Long) extends Error

  final case class ConnectionError(message: String) extends Error

}

trait UserRepos[F[_]] {
  implicit def F: Monad[F]

  def userRepo: UserRepo

  trait UserRepo {
    def followers(userId: Long): F[List[User]]
  }

}


import scala.concurrent.{Future, ExecutionContext, Await}

import scala.concurrent.duration.Duration

class UserRepos0(implicit ec: ExecutionContext) extends UserRepos[Future] {
  override val F = implicitly[Monad[Future]]
  override val userRepo: UserRepo = new UserRepo0 {}

  trait UserRepo0 extends UserRepo {
    def followers(userId: Long): Future[List[User]] = Future.successful {Nil}
  }

}

val service = new UserRepos0()(ExecutionContext.global)

val xs = service.userRepo.followers(1L)


class TestUserRepos extends UserRepos[Id] {
  override val F = implicitly[Monad[Id]]
  override val userRepo: UserRepo = new UserRepo0 {}

  trait UserRepo0 extends UserRepo {
    def followers(userId: Long): List[User] =
      userId match {
        case 0L => List(User(1, "Michael"))
        case 1L => List(User(0, "Vito"))
        case x => sys.error("not found")
      }
  }

}

val testRepo = new TestUserRepos {}

testRepo.userRepo.followers(1L)

/*
trait UserServices[F[_]] {
  this: UserRepos[F] =>
  def userService: UserService = new UserService

  class UserService {
    def isFriends(user1: Long, user2: Long): F[Boolean] =
      F.flatMap(userRepo.followers(user1)) { a =>
        F.map(userRepo.followers(user2)) { b =>
          a.exists(_.id == user2) && b.exists(_.id == user1)
        }
      }
  }

}
*/

val testService = new TestUserRepos with UserServices[Id] {}

testService.userService.isFriends(0L, 1L)

trait UserServices[F[_]] {
  this: UserRepos[F] =>
  def userService: UserService = new UserService

  class UserService {

    import example.MonadSyntax._

    def isFriends(user1: Long, user2: Long): F[Boolean] =
      actM[F, Boolean] {
        val a = userRepo.followers(user1).next
        val b = userRepo.followers(user2).next
        a.exists(_.id == user2) && b.exists(_.id == user1)
      }
  }

}

val testService1 = new TestUserRepos with UserServices[Id] {}

testService1.userService.isFriends(0L, 1L)

class UserRepos1(implicit ec: ExecutionContext) extends UserRepos[EitherT[Future, Error, ?]] {
  override val F = implicitly[Monad[EitherT[Future, Error, ?]]]
  override val userRepo: UserRepo = new UserRepo1 {}

  trait UserRepo1 extends UserRepo {
    def followers(userId: Long): EitherT[Future, Error, List[User]] =
      userId match {
        case 0L => EitherT.right(Future {List(User(1, "Michael"))})
        case 1L => EitherT.right(Future {List(User(0, "Vito"))})
        case x =>
          EitherT.left(Future.successful {Error.UserNotFound(x)})
      }
  }

}

val service1 = {
  import ExecutionContext.Implicits._
  new UserRepos1 with UserServices[EitherT[Future, Error, ?]] {}
}

{
  import scala.concurrent.duration._
  Await.result(service1.userService.isFriends(0L, 1L).value, 1 second)
}

{
  import scala.concurrent.duration._
  Await.result(service1.userService.isFriends(0L, 2L).value, 1 second)
}
