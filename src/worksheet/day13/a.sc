import cats._, cats.data._, cats.implicits._

val one: Id[Int] = 1

Functor[Id].map(one) {_ + 1}

Apply[Id].ap({_ + 1}: Id[Int => Int])(one)

FlatMap[Id].flatMap(one) {_ + 1}
