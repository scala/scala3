type Pos = {x: Int with x > 0}

type NonEmptyString = {s: String with !s.isEmpty}
type PoliteString = {s: NonEmptyString with s.head.isUpper && s.takeRight(6) == "please"}

def id[T](x: T): T =
  println("call id")
  x

def rec(x: NonEmptyString): List[Char] =
  val rest =
    x.tail match
      case xs: NonEmptyString => rec(xs)
      case _ => Nil

  x.head :: rest

@main def Test =
  for v <- List[Any](-1, 1, 2, "", "Do it please", "do it already", false, null) do
    val vStr =
      if v.isInstanceOf[String] then s""""$v""""
      else if v == null then "null"
      else v.toString

    v match
      case _: {v: Int with v == 2} => println(s"$vStr is {v: Int with v == 2}")
      case _: Pos => println(s"$vStr is Pos")
      case _: PoliteString => println(s"$vStr is PoliteString")
      case _: NonEmptyString => println(s"$vStr is NonEmptyString")
      case _ => println(s"$vStr is none of the above")

