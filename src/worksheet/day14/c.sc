import cats._, cats.data._, cats.implicits._

sealed trait Character

case object Farmer extends Character

case object Wolf extends Character

case object Goat extends Character

case object Cabbage extends Character

case class Move(x: Character)

case class Plan(moves: List[Move])

sealed trait Position

case object West extends Position

case object East extends Position

implicit val moveShow = Show.show[Move](_ match {
  case Move(Farmer) => "F"
  case Move(Wolf) => "W"
  case Move(Goat) => "G"
  case Move(Cabbage) => "C"
})

val possibleMoves = List(Farmer, Wolf, Goat, Cabbage) map (Move(_))

/*
def makeMove(ps: List[List[Move]]): List[List[Move]] =
  (ps |@| possibleMoves) map { (p, m) => List(m) <+> p }
*/

/*
def makeMoves(n: Int): List[List[Move]] = n match {
  case 0 => Nil
  case 1 => makeMove(List(Nil))
  case _ => makeMove(makeMoves(n - 1))
}
*/

def filterA[F[_] : Alternative : Monad, A](fa: F[A])(cond: A => Boolean): F[A] = {
  val A = Alternative[F]
  val M = Monad[F]
  M.flatMap(fa) { a =>
    if (cond(a)) A.pure(a)
    else A.empty
  }
}

def positionOf(p: List[Move], c: Character): Position = {
  def positionFromCount(n: Int): Position = {
    if (n % 2 == 0) West
    else East
  }
  c match {
    case Farmer => positionFromCount(p.size)
    case _ => positionFromCount(filterA(p)(_ == Move(c)).size)
  }
}

val p = List(Move(Goat), Move(Farmer), Move(Wolf), Move(Goat))

positionOf(p, Farmer)

def isSolution(p: List[Move]) = {
  val pos = (List(p) |@| possibleMoves) map { (p, m) => positionOf(p, m.x) }
  filterA(pos)(_ == West).isEmpty
}

def moveLegal(p: List[Move], m: Move): Boolean =
  positionOf(p, Farmer) == positionOf(p, m.x)


moveLegal(p, Move(Wolf))

def safePlan(p: List[Move]): Boolean = {
  val posGoat = positionOf(p, Goat)
  val posFarmer = positionOf(p, Farmer)
  val safeGoat = posGoat != positionOf(p, Wolf)
  val safeCabbage = positionOf(p, Cabbage) != posGoat
  (posFarmer == posGoat) || (safeGoat && safeCabbage)
}

def makeMove(ps: List[List[Move]]): List[List[Move]] =
  (ps |@| possibleMoves) map { (p, m) =>
    if (!moveLegal(p, m)) Nil
    else if (!safePlan(List(m) <+> p)) Nil
    else List(m) <+> p
  }

def makeNMoves(n: Int): List[List[Move]] =
  n match {
    case 0 => Nil
    case 1 => makeMove(List(Nil))
    case n => makeMove(makeNMoves(n - 1))
  }

def findSolution(n: Int): Unit =
  filterA(makeNMoves(n))(isSolution) map { p =>
    println(p map {_.show})
  }

findSolution(6)

findSolution(7)

findSolution(8)
