import cats._, cats.data._, cats.implicits._

List(1, 2, 3) |+| List(4, 5, 6)

"one" |+| "two"

case class Foo(x: String)

//Foo("x").some |+| Foo("y").some

implicit val semigroupFoo = SemigroupK[Option].algebra[Foo]

Foo("x").some <+> Foo("y").some

List(Foo("x")) <+> List(Foo("y"))

Foo("x").some |+| Foo("y").some


case class Foo1[A](a: A)

implicit def semigroupFoo1[A: Semigroup] = new Semigroup[Foo1[A]] {
  override def combine(x: Foo1[A], y: Foo1[A]): Foo1[A] = {
    Foo1(x.a combine y.a)
  }
}

Foo1(1) |+| Foo1(2)

Foo1("1") |+| Foo1("2")
