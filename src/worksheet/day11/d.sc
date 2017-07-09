import cats._, cats.data._, cats.implicits._

val x = Prod(List(1), 1.some)

Functor[Prod[List, Option, ?]].map(x) {_ + 1}

val x1 = Prod(List(1), 1.some)

val f1 = Prod(List((_: Int) + 1), Some((_: Int) * 3): Option[Int => Int])

Apply[Prod[List, Option, ?]].ap(f1)(x1)

Applicative[Prod[List, Option, ?]].pure(1)

Applicative[List].compose[Option].pure(1)

val f2 = Func.appFunc { x: Int => List(x.toString + "!") }

val g2 = Func.appFunc { x: Int => Some(x.toString + "?"): Option[String] }

val h2 = f2 product g2

h2.run(1)

val f3 = Func.appFunc { x: Int => List(x.toString + "!") }

val g3 = Func.appFunc { x: String => Some(x + "?"): Option[String] }

val h3 = f3 andThen g3

h3.run(1)