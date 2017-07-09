import cats._, cats.data._, cats.implicits._
import annotation.tailrec

case class Prob[A](list: List[(A, Double)])

trait ProbInstances { self =>
  /*
  implicit val probFunctor: Functor[Prob] = new Functor[Prob] {
    def map[A, B](fa: Prob[A])(f: A => B): Prob[B] =
      Prob(fa.list map { case (x, p) => (f(x), p) })
  }
  */

  def flatten[B](prob: Prob[Prob[B]]): Prob[B] = {
    def multAll(innerProb: Prob[B], d: Double) = {
      innerProb.list map { case (a, d1) => (a, d1 * d) }
    }
    Prob(prob.list flatMap ((multAll _).tupled(_)))
  }

  implicit val probMonad: Monad[Prob] = new Monad[Prob] {
    def pure[A](a: A): Prob[A] = Prob(List((a, 1.0)))

    def flatMap[A, B](fa: Prob[A])(f: A => Prob[B]): Prob[B] = self.flatten(map(fa)(f))

    override def map[A, B](fa: Prob[A])(f: A => B): Prob[B] = {
      Prob(fa.list map { case (a, d) => (f(a), d) })
    }

    def tailRecM[A, B](a: A)(f: A => Prob[Either[A, B]]): Prob[B] = {
      @tailrec
      def go(lists: List[List[(Either[A, B], Double)]], result: Vector[(B, Double)]): Vector[(B, Double)] = lists match {
        case (innerHead :: innerTail) :: tail => innerHead match {
          case (Right(a), d) => go(innerTail :: tail, result :+ (a, d))
          case (Left(a), d) => go(f(a).list :: innerTail :: tail, result)
        }
        case Nil :: tail => go(tail, result)
        case Nil => result
      }
      Prob(go(f(a).list :: Nil, Vector()).toList)
    }
  }

  implicit def probShow[A]: Show[Prob[A]] = Show.fromToString
}

case object Prob extends ProbInstances

//Prob(List(3 -> 0.5, 5 -> 0.25, 9 -> 0.25)) map (-_)
