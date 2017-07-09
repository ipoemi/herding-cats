sealed abstract class Fix[S[_], A] extends Serializable {
  def out: S[Fix[S, A]]
}

object Fix {

  case class In[S[_], A](out: S[Fix[S, A]]) extends Fix[S, A]

}

sealed trait ListF[+Next, +A]

object ListF {

  case class NilF() extends ListF[Nothing, Nothing]

  case class ConsF[A, Next](a: A, n: Next) extends ListF[Next, A]

}

type GenericList[A] = Fix[ListF[+?, A], A]

object GenericList {
  def nil[A]: GenericList[A] = Fix.In[ListF[+?, A], A](ListF.NilF())
  def cons[A](a: A, xs: GenericList[A]): GenericList[A] =
    Fix.In[ListF[+?, A], A](ListF.ConsF(a, xs))
}

import GenericList._

cons(1, nil)

import cats._, cats.data._, cats.implicits._

import cats.functor.Bifunctor

implicit val listFBifunctor: Bifunctor[ListF] = new Bifunctor[ListF] {
  def bimap[S1, A1, S2, A2](fab: ListF[S1, A1])(f: S1 => S2, g: A1 => A2): ListF[S2, A2] =
    fab match {
      case ListF.NilF() => ListF.NilF()
      case ListF.ConsF(a, next) => ListF.ConsF(g(a), f(next))
    }
}

object DGP {
  def map[F[_, _] : Bifunctor, A1, A2](fa: Fix[F[?, A1], A1])(f: A1 => A2): Fix[F[?, A2], A2] =
    Fix.In[F[?, A2], A2](Bifunctor[F].bimap(fa.out)(map(_)(f), f))

  // catamorphism
  def fold[F[_, _] : Bifunctor, A1, A2](fa: Fix[F[?, A1], A1])(f: F[A2, A1] => A2): A2 = {
    val g = (fa1: F[Fix[F[?, A1], A1], A1]) =>
      Bifunctor[F].leftMap(fa1) {
        (fold(_)(f))
      }
    f(g(fa.out))
  }

  // anamorphism
  def unfold[F[_, _] : Bifunctor, A1, A2](x: A2)(f: A2 => F[A2, A1]): Fix[F[?, A1], A1] =
    Fix.In[F[?, A1], A1](Bifunctor[F].leftMap(f(x))(unfold[F, A1, A2](_)(f)))
}

DGP.map(cons(1, nil))(_ + 1)

trait FixInstances {
  implicit def fixFunctor[F[_, _] : Bifunctor]: Functor[Lambda[L => Fix[F[?, L], L]]] =
    new Functor[Lambda[L => Fix[F[?, L], L]]] {
      def map[A1, A2](fa: Fix[F[?, A1], A1])(f: A1 => A2): Fix[F[?, A2], A2] =
        Fix.In[F[?, A2], A2](Bifunctor[F].bimap(fa.out)(map(_)(f), f))
    }
}

val instances = new FixInstances {}

import instances._
//import cats.syntax.functor._

cons(1, nil) map (_ + 1)

DGP.fold[ListF, Int, Int](cons(2, cons(1, nil))) {
  case ListF.NilF() => 0
  case ListF.ConsF(x, n) => x + n
}

def pred(n: Int): GenericList[Int] =
  DGP.unfold[ListF, Int, Int](n) {
    case 0 => ListF.NilF()
    case n => ListF.ConsF(n, n - 1)
  }

pred(4)

sealed trait TreeF[+Next, +A]

object TreeF {

  case class EmptyF() extends TreeF[Nothing, Nothing]

  case class NodeF[Next, A](a: A, left: Next, right: Next) extends TreeF[Next, A]

}

type Tree[A] = Fix[TreeF[?, A], A]

object Tree {
  def empty[A]: Tree[A] =
    Fix.In[TreeF[+?, A], A](TreeF.EmptyF())
  def node[A, Next](a: A, left: Tree[A], right: Tree[A]): Tree[A] =
    Fix.In[TreeF[+?, A], A](TreeF.NodeF(a, left, right))
}

import Tree.{empty, node}

node(2, node(1, empty, empty), empty)


implicit val treeFBifunctor: Bifunctor[TreeF] = new Bifunctor[TreeF] {
  def bimap[A, B, C, D](fab: TreeF[A, B])(f: A => C, g: B => D): TreeF[C, D] =
    fab match {
      case TreeF.EmptyF() => TreeF.EmptyF()
      case TreeF.NodeF(a, left, right) =>
        TreeF.NodeF(g(a), f(left), f(right))
    }
}

node(2, node(1, empty, empty), empty) map (_ + 1)

def sum(tree: Tree[Int]): Int =
  DGP.fold[TreeF, Int, Int](tree) {
    case TreeF.EmptyF() => 0
    case TreeF.NodeF(a, l, r) => a + l + r
  }


def grow[A: PartialOrder](xs: List[A]): Tree[A] =
  DGP.unfold[TreeF, A, List[A]](xs) {
    case Nil => TreeF.EmptyF()
    case x :: xs =>
      import cats.syntax.partialOrder._
      TreeF.NodeF(x, xs filter (_ <= x), xs filter (_ > x))
  }

 grow(List(3, 1, 4, 2))

