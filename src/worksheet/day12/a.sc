import cats._, cats.data._, cats.implicits._

List(1, 2, 3) traverse[Id, Int] { (x: Int) => x + 1 }

List(1, 2, 3) traverse { (x: Int) => (x + 1).some }

List(1, 2, 3) traverse { (x: Int) => none[String] }

def reduce[F[_], A, B](fa: F[A])(f: A => B)
  (implicit FF: Traverse[F], BB: Monoid[B]): B = {
  val g: A => Const[B, Unit] = { (a: A) => Const((f(a))) }
  val x = FF.traverse[Const[B, ?], A, Unit](fa)(g)
  x.getConst
}

reduce(List('a', 'b', 'c')) { c: Char => c.toInt }

def reduce2[A, B, F[_]](fa: F[A])(f: A => B)
  (implicit FF: Traverse[F], BB: Monoid[B]): B = {
  val x = fa traverseU { (a: A) => Const((f(a))) }
  x.getConst
}

reduce2(List('a', 'b', 'c')) { c: Char => c.toInt }

import scala.concurrent.{ Future, ExecutionContext, Await }

import scala.concurrent.duration._

val x = {
  implicit val ec = scala.concurrent.ExecutionContext.global
  List(Future { 1 }, Future { 2 }).sequence
}

Await.result(x, 2 seconds)

List(Right(1): Either[String, Int]).sequenceU

List(Right(1): Either[String, Int], Left("boom"): Either[String, Int]).sequenceU

