import cats._, cats.data._, cats.implicits._

def contents[F[_], A](fa: F[A])(implicit FF: Traverse[F]): Const[List[A], F[Unit]] = {
  val contentsBody: A => Const[List[A], Unit] = { (a: A) => Const(List(a)) }
  FF.traverseU(fa)(contentsBody)
}

contents(Vector(1, 2, 3)).getConst

def shape[F[_], A](fa: F[A])(implicit FF: Traverse[F]): Id[F[Unit]] = {
  val shapeBody: A => Id[Unit] = { (a: A) => () }
  FF.traverseU(fa)(shapeBody)
}

shape(Vector(1, 2, 3))

def decompose[F[_], A](fa: F[A])(implicit FF: Traverse[F]) =
  Prod[Const[List[A], ?], Id, F[Unit]](contents(fa), shape(fa))

val d = decompose(Vector(1, 2, 3))

d.first

d.second

import cats.data.Func.appFunc

def contentsBody[A]: AppFunc[Const[List[A], ?], A, Unit] =
  appFunc[Const[List[A], ?], A, Unit] { (a: A) => Const(List(a)) }

def shapeBody[A]: AppFunc[Id, A, Unit] =
  appFunc { (a: A) => ((): Id[Unit]) }

def decompose2[F[_], A](fa: F[A])(implicit FF: Traverse[F]) =
  (contentsBody[A] product shapeBody[A]).traverse(fa)
