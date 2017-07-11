import cats._, cats.data._, cats.implicits._

val f = (_: Int) + 1

val g = (_: Int) * 100

(f >>> g)(2)

(f <<< g)(2)

val f_first = f.first[Int]

val f_second = f.second[Int]

val g_first = g.first[Int]

val g_second = g.first[Int]

f_first((1, 1))

f_second((1, 1))

g_first((1, 1))

g_second((1, 1))

val f_split_g = (f split g)

f_split_g((1, 1))
