type Pos = {x: Int with x > 0}

type NonEmptyString = {s: String with !s.isEmpty}
type PoliteString = {s: NonEmptyString with s.head.isUpper && s.takeRight(6) == "please"}

def id[T](x: T): T =
  println("call id")
  x

@main
def Test =
  for v <- List[Any](-1, 1, 2, "", "Do it please", "do it already", false, null) do
    val vStr =
      if v.isInstanceOf[String] then s""""$v""""
      else if v == null then "null"
      else v.toString
    println(s"$vStr is instance of Pos: ${id(v).isInstanceOf[Pos]}")
    println(s"$vStr is instance of {v: Int with v == 2}: ${id(v).isInstanceOf[{v: Int with v == 2}]}")
    println(s"$vStr is instance of NonEmptyString: ${id(v).isInstanceOf[NonEmptyString]}")
    println(s"$vStr is instance of PoliteString: ${id(v).isInstanceOf[PoliteString]}")
    println(s"$vStr is instance of Pos & Int: ${id(v).isInstanceOf[Pos & Int]}")
    println(s"$vStr is instance of Pos | Int: ${id(v).isInstanceOf[Pos | Int]}")
    println(s"$vStr is instance of Pos & String: ${id(v).isInstanceOf[Pos & String]}")
    println(s"$vStr is instance of Pos | String: ${id(v).isInstanceOf[Pos | String]}")
    println(s"$vStr is instance of {v: Int with v == 2} & Int: ${id(v).isInstanceOf[{v: Int with v == 2} & Int]}")
    println(s"$vStr is instance of {v: Int with v == 2} | Int: ${id(v).isInstanceOf[{v: Int with v == 2} | Int]}")
    println(s"$vStr is instance of {v: Int with v == 2} & String: ${id(v).isInstanceOf[{v: Int with v == 2} & String]}")
    println(s"$vStr is instance of {v: Int with v == 2} | String: ${id(v).isInstanceOf[{v: Int with v == 2} | String]}")
    println(s"$vStr is instance of NonEmptyString & String: ${id(v).isInstanceOf[NonEmptyString & String]}")
    println(s"$vStr is instance of NonEmptyString | String: ${id(v).isInstanceOf[NonEmptyString | String]}")
    println(s"$vStr is instance of NonEmptyString & Int: ${id(v).isInstanceOf[NonEmptyString & Int]}")
    println(s"$vStr is instance of NonEmptyString | Int: ${id(v).isInstanceOf[NonEmptyString | Int]}")
    println(s"$vStr is instance of PoliteString & Int: ${id(v).isInstanceOf[PoliteString & Int]}")
    println(s"$vStr is instance of PoliteString | Int: ${id(v).isInstanceOf[PoliteString | Int]}")
    println(s"$vStr is instance of PoliteString & String: ${id(v).isInstanceOf[PoliteString & String]}")
    println(s"$vStr is instance of PoliteString | String: ${id(v).isInstanceOf[PoliteString | String]}")
