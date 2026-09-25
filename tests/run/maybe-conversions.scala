// scalajs: --skip
// scalajs needs to be disabled since `null` gives an Err(undefined) instead of an Err(())
//> using options -Yexplicit-nulls
import language.experimental.errorHandling

import scala.util.{Either, Left, Right, Ok, Err}

def show[T, E](x: T ? E): String = x match
  case Ok(y) => s"Ok($y)"
  case Err(e) => s"Err($e)"

def showOpt[T](x: T?): String = x match
  case Ok(y) => s"Ok($y)"
  case null => "null"

val nullStr: String | Null = null

// ---- Maybe => Option

def maybeToOption() =
  println("==== Maybe.toOption")
  val m1: Int? = 1
  val m2: Int? = null
  val m3: String? = "abc"
  val m4: String? = null
  val m5: Int? = Ok(2)
  val m6: Int? = Err(())
  println(m1.toOption)
  println(m2.toOption)
  println(m3.toOption)
  println(m4.toOption)
  println(m5.toOption)
  println(m6.toOption)
  assert(m1.toOption == Some(1))
  assert(m2.toOption == None)
  assert(m3.toOption == Some("abc"))
  assert(m4.toOption == None)
  assert(m5.toOption == Some(2))
  assert(m6.toOption == None)

  // nested: a Maybe holding null or an Err
  val n1: (String | Null)? = Ok(nullStr)
  val n2: (Int ? String)? = Ok(Err("bad"))
  val n3: (Int ? String)? = Ok(Ok(3))
  println(n1.toOption)
  println(n2.toOption)
  println(n3.toOption)
  assert(n1.toOption == Some(null))
  assert(n2.toOption.get.toEither == Left("bad"))
  assert(n3.toOption.get.toEither == Right(3))

// ---- Maybe => Either

def maybeToEither() =
  println("==== Maybe.toEither")
  val m1: Int ? String = 1
  val m2: Int ? String = Err("bad")
  val m3: Int ? String = Ok(2)
  val m4: String ? Int = "abc"
  val m5: String ? Int = Err(0)
  val m6: Int? = 3
  val m7: Int? = null
  println(m1.toEither)
  println(m2.toEither)
  println(m3.toEither)
  println(m4.toEither)
  println(m5.toEither)
  println(m6.toEither)
  println(m7.toEither)
  assert(m1.toEither == Right(1))
  assert(m2.toEither == Left("bad"))
  assert(m3.toEither == Right(2))
  assert(m4.toEither == Right("abc"))
  assert(m5.toEither == Left(0))
  assert(m6.toEither == Right(3))
  assert(m7.toEither == Left(()))

  // nested: a Maybe holding null or an Err
  val n1: (String | Null) ? String = Ok(nullStr)
  val n2: (Int ? String) ? String = Ok(Err("inner"))
  val n3: (Int ? String) ? String = Err("outer")
  println(n1.toEither)
  println(n2.toEither.map(show))
  println(n3.toEither)
  assert(n1.toEither == Right(null))
  assert(n2.toEither.map(show) == Right("Err(inner)"))
  assert(n3.toEither == Left("outer"))

// ---- Option => Maybe

def optionToMaybe() =
  println("==== Option.toMaybe")
  val o1: Option[Int] = Some(1)
  val o2: Option[Int] = None
  val o3: Option[String] = Some("abc")
  val o4: Option[String] = None
  println(showOpt(o1.toMaybe))
  println(showOpt(o2.toMaybe))
  println(showOpt(o3.toMaybe))
  println(showOpt(o4.toMaybe))
  assert(!o1.toMaybe.isEmpty)
  assert(o2.toMaybe.isEmpty)
  assert(o2.toMaybe == null)
  assert(!o3.toMaybe.isEmpty)
  assert(o4.toMaybe.isEmpty)
  assert(o4.toMaybe == null)

  // nested: an Option holding null or a Maybe
  val n1: Option[String | Null] = Some(null)
  val n2: Option[Int?] = Some(null)
  val n3: Option[Int ? String] = Some(Err("bad"))
  val n4: Option[Int ? String] = Some(Ok(4))
  println(showOpt(n1.toMaybe))
  println(showOpt(n2.toMaybe))
  println(showOpt(n3.toMaybe))
  println(showOpt(n4.toMaybe))
  assert(!n1.toMaybe.isEmpty)
  assert(!n2.toMaybe.isEmpty)
  assert(!n3.toMaybe.isEmpty)
  assert(!n4.toMaybe.isEmpty)
  n1.toMaybe match
    case Ok(x) => assert(x == null)
    case null => assert(false)
  n2.toMaybe match
    case Ok(x) => assert(x == null)
    case null => assert(false)
  n3.toMaybe match
    case Ok(x) => assert(x.toEither == Left("bad"))
    case null => assert(false)
  n4.toMaybe match
    case Ok(x) => assert(x.toEither == Right(4))
    case null => assert(false)

// ---- Either => Maybe

def eitherToMaybe() =
  println("==== Either.toMaybe")
  val e1: Either[String, Int] = Right(1)
  val e2: Either[String, Int] = Left("bad")
  val e3: Either[Int, String] = Right("abc")
  val e4: Either[Int, String] = Left(0)
  println(showOpt(e1.toMaybe))
  println(showOpt(e2.toMaybe))
  println(showOpt(e3.toMaybe))
  println(showOpt(e4.toMaybe))
  assert(!e1.toMaybe.isEmpty)
  assert(e2.toMaybe.isEmpty)
  assert(e2.toMaybe == null)
  assert(!e3.toMaybe.isEmpty)
  assert(e4.toMaybe.isEmpty)
  assert(e4.toMaybe == null)

  // nested: an Either holding null or a Maybe on the right
  val n1: Either[String, String | Null] = Right(null)
  val n2: Either[String, Int?] = Right(null)
  val n3: Either[String, Int ? String] = Right(Err("inner"))
  println(showOpt(n1.toMaybe))
  println(showOpt(n2.toMaybe))
  println(showOpt(n3.toMaybe))
  assert(!n1.toMaybe.isEmpty)
  assert(!n2.toMaybe.isEmpty)
  assert(!n3.toMaybe.isEmpty)

// ---- Either => Result (Maybe with error)

def eitherToResult() =
  println("==== Either.toResult")
  val e1: Either[String, Int] = Right(1)
  val e2: Either[String, Int] = Left("bad")
  val e3: Either[Int, String] = Right("abc")
  val e4: Either[Int, String] = Left(0)
  val e5: Either[Unit, Int] = Left(())
  println(show(e1.toResult))
  println(show(e2.toResult))
  println(show(e3.toResult))
  println(show(e4.toResult))
  println(show(e5.toResult))
  assert(!e1.toResult.isEmpty)
  assert(e2.toResult.isEmpty)
  assert(!e3.toResult.isEmpty)
  assert(e4.toResult.isEmpty)
  assert(e5.toResult.isEmpty)
  assert(e5.toResult == null)  // a Unit error is represented as null

  // nested: an Either holding null or a Maybe on either side
  val n1: Either[String, String | Null] = Right(null)
  val n2: Either[String, Int ? String] = Right(Err("inner"))
  val n3: Either[Int ? String, Int] = Left(Err("inner"))
  val n4: Either[Int ? String, Int] = Left(Ok(5))
  println(show(n1.toResult))
  println(show(n2.toResult))
  println(show(n3.toResult))
  println(show(n4.toResult))
  assert(!n1.toResult.isEmpty)
  assert(!n2.toResult.isEmpty)
  assert(n3.toResult.isEmpty)
  assert(n4.toResult.isEmpty)
  n2.toResult match
    case Ok(x) => assert(x.toEither == Left("inner"))
    case Err(_) => assert(false)
  n3.toResult match
    case Ok(_) => assert(false)
    case Err(e) => assert(e.toEither == Left("inner"))
  n4.toResult match
    case Ok(_) => assert(false)
    case Err(e) => assert(e.toEither == Right(5))

// ---- Round trips

def roundTrips() =
  println("==== round trips")
  val opts: List[Option[Int]] = List(Some(1), None)
  for o <- opts do
    assert(o.toMaybe.toOption == o)
    assert(o.toMaybe.toEither == o.toRight(()))
  val eithers: List[Either[String, Int]] = List(Right(1), Left("bad"))
  for e <- eithers do
    assert(e.toResult.toEither == e)
    assert(e.toMaybe.toOption == e.toOption)
  val maybes: List[Int ? String] = List(Ok(1), Err("bad"))
  for m <- maybes do
    assert(m.toEither.toResult == m)
  val opts2: List[Int?] = List(Ok(1), null)
  for m <- opts2 do
    assert(m.toOption.toMaybe == m)
    assert(m.toEither.toMaybe == m)
  println("ok")

@main def Test =
  maybeToOption()
  maybeToEither()
  optionToMaybe()
  eitherToMaybe()
  eitherToResult()
  roundTrips()
