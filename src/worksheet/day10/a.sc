case class User(id: Long, parentId: Long, name: String, email: String)

/*
trait UserRepo {
  def get(id: Long): User

  def find(name: String): User
}
*/

trait UserRepo {
  def get(id: Long): Option[User]

  def find(name: String): Option[User]
}

import java.net.URI

trait HttpService {
  def get(uri: URI): String
}

trait Config {
  def userRepo: UserRepo

  def httpService: Option[HttpService]
}

import cats._, cats.data._, cats.implicits._

type ReaderTOption[A, B] = Kleisli[Option, A, B]

object ReaderTOption {
  def ro[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
}

trait Users {
  def getUser(id: Long): ReaderTOption[Config, User] =
    ReaderTOption.ro(config => config.userRepo.get(id))

  def findUser(name: String): ReaderTOption[Config, User] =
    ReaderTOption.ro(config => config.userRepo.find(name))
}

trait Https {
  def getHttp(uri: URI): ReaderTOption[Config, String] =
    ReaderTOption.ro(config => config.httpService.map(_.get(uri)))
}

trait Program extends Users with Https {
  def userSearch(id: Long): ReaderTOption[Config, String] =
    for {
      user <- getUser(id)
      result <- getHttp(new URI(s"http://www.google.com/?q=${user.name}"))
    } yield result
}

object Main extends Program {
  def run(config: Config): Option[String] =
    userSearch(2).run(config)
}

val dummyConfig: Config = new Config {
  val testUsers = List(
    User(0, 0, "Vito", "vito@example.com"),
    User(1, 0, "Michael", "michael@example.com"),
    User(2, 0, "Fredo", "fredo@example.com")
  )

  def userRepo: UserRepo = new UserRepo {
    def get(id: Long): Option[User] =
      testUsers find {
        _.id === id
      }

    def find(name: String): Option[User] =
      testUsers find {
        _.name === name
      }
  }

  def httpService: Option[HttpService] = new HttpService {
    override def get(uri: URI): String = uri.toString
  }.some
}

Main.run(dummyConfig)



type StateTReaderTOption[C, S, A] = StateT[ReaderTOption[C, ?], S, A]

object StateTReaderTOption {
  def state[C, S, A](f: S => (S, A)): StateTReaderTOption[C, S, A] =
    StateT[ReaderTOption[C, ?], S, A] {
      s: S => Monad[ReaderTOption[C, ?]].pure(f(s))
    }

  def get[C, S]: StateTReaderTOption[C, S, S] =
    state { s => (s, s) }

  def put[C, S](s: S): StateTReaderTOption[C, S, Unit] =
    state { _ => (s, ()) }

  def ro[C, S, A](f: C => Option[A]): StateTReaderTOption[C, S, A] =
    StateT[ReaderTOption[C, ?], S, A] {
      s: S =>
        ReaderTOption.ro[C, (S, A)] {
          c: C =>
            f(c) map {
              (s, _)
            }
        }
    }
}

type Stack = List[String]

/*
val pop = StateTReaderTOption.state {
  case x :: xs => (xs, x)
  case _ => ???
}
*/

import StateTReaderTOption.get

val pop: StateTReaderTOption[Config, Stack, String] = for {
  s <- StateTReaderTOption.get[Config, Stack]
  (x :: xs) = s
  _ <- StateTReaderTOption.put(xs)
} yield x

def push(x: String): StateTReaderTOption[Config, Stack, Unit] = for {
  xs <- StateTReaderTOption.get[Config, Stack]
  r <- StateTReaderTOption.put(x :: xs)
} yield r

def stackManip: StateTReaderTOption[Config, Stack, String] =
  for {
    _ <- push("Fredo")
    a <- pop
    b <- pop
  } yield (b)

stackManip.run(List("Hyman Roth")).run(dummyConfig)


trait Users2 {
  def getUser[S](id: Long): StateTReaderTOption[Config, S, User] =
    StateTReaderTOption.ro(config => config.userRepo.get(id))

  def findUser[S](name: String): StateTReaderTOption[Config, S, User] =
    StateTReaderTOption.ro(config => config.userRepo.find(name))
}


trait Program2 extends Users2 {
  def stackManip: StateTReaderTOption[Config, Stack, Unit] =
    for {
      u <- getUser(2)
      a <- push(u.name)
    } yield (a)
}

object Main2 extends Program2 {
  def run(s: Stack, config: Config): Option[(Stack, Unit)] =
    stackManip.run(s).run(config)
}

Main2.run(List("Hyman Roth"), dummyConfig)
