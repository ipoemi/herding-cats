import cats._, cats.data._, cats.implicits._

var g = 0

val x = Eval.later {
  g = g + 1
  g
}

g = 2

x.value

x.value

val y = Eval.now {
  g = g + 1
  g
}

y.value

y.value

val z = Eval.always {
  g = g + 1
  g
}

z.value

z.value

object OddEven0 {
  def odd(n: Int): String = even(n - 1)
  def even(n: Int): String = if (n <= 0) "done" else odd(n - 1)
}

object OddEven1 {
  def odd(n: Int): Eval[String] = Eval.defer {even(n - 1)}
  def even(n: Int): Eval[String] =
    Eval.now { n <= 0 } flatMap {
      case true => Eval.now {"done"}
      case _    => Eval.defer { odd(n - 1) }
    }
}
OddEven1.even(200000).value
