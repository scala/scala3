class Arg[T]

// Traditional scheme: use location in class hierarchy

class E[T](val str: String)

class LowPriorityImplicits {
  implied t1[T] for E[T]("low")
}

object NormalImplicits extends LowPriorityImplicits {
  implied t2[T] given Arg[T] for E[T]("norm")
}

def test1 = {
  import implied NormalImplicits._
  assert(the[E[String]].str == "low")

  { implied for Arg[String]
    assert(the[E[String]].str == "norm")
  }
}

// Priority arguments:

object Priority {
  class Low
  object Low { implied for Low }
  class High extends Low
  object High { implied for High }
}

object Impl2 {
  implied t1[T] given Priority.Low for E[T]("low")
  implied t2[T] given Priority.High given Arg[T] for E[T]("norm")
}

def test2 = {
  import implied Impl2._
  assert(the[E[String]].str == "low")

  { implied for Arg[String]
    assert(the[E[String]].str == "norm")
  }
}

// Adding an override to an existing hierarchy:
// If all of the alternatives in the existing hierarchy take implicit arguments,
// an alternative without implicit arguments would override all of them.

object Impl2a {
  implied t3[T] for E[T]("hi")
}

def test2a = {
  import implied Impl2._
  import implied Impl2a._

  implied for Arg[String]
  assert(the[E[String]].str == "hi")
}

// If not, we can use result refinement:

object Impl3 {
  implied t1[T] for E[T]("low")
}

object Override {
  trait HighestPriority

  implied over[T] for E[T]("hi"), HighestPriority
}

def test3 = {
  import implied Impl3._
  assert(the[E[String]].str == "low")

  { import implied Override._
    assert(the[E[String]].str == "hi")
  }
}

// Adding a fallback to an existing hierarchy:
object Impl4 {
  implied t1 for E[String]("string")
  implied t2[T] given Arg[T] for E[T]("generic")
}

object fb {
  def withFallback[T] given (ev: E[T] = new E[T]("fallback")): E[T] = ev
  implied [T] given (ev: E[T] = new E[T]("fallback")) for E[T] = ev
}

def test4 = {
  import implied Impl4._
  import fb._
  assert(withFallback[String].str == "string")
  assert(withFallback[Int].str == "fallback")

  { implied for Arg[Int]
    assert(withFallback[Int].str == "generic")
  }
}


object Test extends App {
  test1
  test2
  test2a
  test3
  test4
}

