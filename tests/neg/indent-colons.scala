def tryEither[T](x: T)(y: Int => T): T = ???

def test1 =
  tryEither:  // error
      "hello"
    :  // error
      y => y.toString

def test2 =
  tryEither:  // error
    "hello"
  :  // error
    _.toString


val o =
  Some(3).fold:  // error
    "nothing"
  :  // error
    x => x.toString

object Test23:
  val x = 1.+ :  // error
    2

  val y = 1 + : // error
    x

  val credentials = List("OK")
  val all = credentials ++ :  // error
    val file = "file"
    if file.isEmpty  // error
    then Seq("none")
    else Seq(file) // error