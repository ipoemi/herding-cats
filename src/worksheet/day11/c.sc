import cats._, cats.data._
import cats.syntax.functor._
import cats.syntax.apply._

Const(1) map ((_: String) + "!")

implicit val productSemigroupInt = new Semigroup[Int] {
  def combine(x: Int, y: Int): Int = x * y
}

Const(2).retag[String => String] ap Const(1).retag[String]
