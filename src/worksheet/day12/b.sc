import cats._, cats.data._, cats.implicits._

List(1, 2, 3) filterA { x => List(true, false) }

Vector(1, 2, 3) filterA { x => Vector(true, false) }
