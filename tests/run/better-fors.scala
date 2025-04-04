//> using options -preview
// import scala.language.experimental.betterFors

def for1 =
  for {
    a = 1
    b <- List(a, 2)
    c <- List(3, 4)
  } yield (b, c)

def for2 =
  for
    a = 1
    b = 2
    c <- List(3, 4)
  yield (a, b, c)

def for3 =
  for {
    a = 1
    b <- List(a, 2)
    c = 3
    d <- List(c, 4)
  } yield (b, d)

def for4 =
  for {
    a = 1
    b <- List(a, 2)
    if b > 1
    c <- List(3, 4)
  } yield (b, c)

def for5 =
  for {
    a = 1
    b <- List(a, 2)
    c = 3
    if b > 1
    d <- List(c, 4)
  } yield (b, d)

def for6 =
  for {
    a = 1
    b = 2
    c <- for {
      x <- List(a, b)
      y = x * 2
    } yield (x, y)
  } yield c

def for7 =
  for {
    a <- List(1, 2, 3)
  } yield a

def for8 =
  for {
    a <- List(1, 2)
    b = a + 1
    if b > 2
    c = b * 2
    if c < 8
  } yield (a, b, c)

def for9 =
  for {
    a <- List(1, 2)
    b = a * 2
    if b > 2
  } yield a + b

def for10 =
  for {
    a <- List(1, 2)
    b = a * 2
  } yield a + b

def for11 =
  for {
    a <- List(1, 2)
    b = a * 2
    if b > 2 && b % 2 == 0
  } yield a + b

def for12 =
  for {
    a <- List(1, 2)
    if a > 1
  } yield a

object Test extends App {
  println(for1)
  println(for2)
  println(for3)
  println(for4)
  println(for5)
  println(for6)
  println(for7)
  println(for8)
  println(for9)
  println(for10)
  println(for11)
  println(for12)
}
