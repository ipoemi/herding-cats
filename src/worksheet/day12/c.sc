import cats._, cats.data._, cats.implicits._

def foo[F[_]: Applicative](fa: F[Int]): F[Int] = fa

//foo(Right(1): Either[String, Int])

def fooU[FA](fa: FA)(implicit U: Unapply[Applicative, FA]): U.M[U.A] = U.subst(fa)

fooU(Right(1): Either[String, Int])

(Right(1): Either[String, Int]) *> Right(2)

(Right(1): Either[String, Int]) <* Right(2)

(Left("Oops"): Either[String, Int]) *> Right(2)

