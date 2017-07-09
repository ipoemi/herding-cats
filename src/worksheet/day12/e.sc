import cats._, cats.data._, cats.implicits._
import Func.{appFunc, appFuncU}

type Count[A] = Const[Int, A]

def liftInt(i: Int): Count[Unit] = Const(i)

def count[A](a: A): Count[Unit] = liftInt(1)

val countChar: AppFunc[Count, Char, Unit] = appFunc(count)

val text = ("Faith, I must leave thee, love, and shortly too.\n" +
  "My operant powers their functions leave to do.\n").toList

countChar traverse text

def testIf(b: Boolean): Int = if (b) 1 else 0

val countLine: AppFunc[Count, Char, Unit] =
  appFunc { (c: Char) => liftInt(testIf(c === '\n')) }

countLine traverse text

def isSpace(c: Char): Boolean = (c === ' ' || c === '\n' || c === '\t')

val countWord =
  appFuncU { (c: Char) =>
    import cats.data.State.{get, set}
    for {
      x <- get[Boolean]
      y = !isSpace(c)
      _ <- set(y)
    } yield testIf(y && !x)
  } andThen appFunc(liftInt)

val x = countWord traverse text
x.value.runA(false).value

val countAll = countWord product countLine product countChar

val allResult = countAll traverse text

val charCount = allResult.second

val lineCount = allResult.first.second

val wordCount = allResult.first.first.value.runA(false).value

