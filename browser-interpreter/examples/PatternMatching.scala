object PatternMatching {
  def describe(x: Any): String = x match {
    case 0 => "zero"
    case 1 => "one"
    case n: Int if n < 0 => "negative"
    case n: Int if n > 100 => "large"
    case s: String => s"string: $s"
    case list: List[_] => s"list with ${list.length} elements"
    case Some(v) => s"Some($v)"
    case None => "None"
    case _ => "something else"
  }

  def main(args: Array[String]): Unit = {
    println(describe(0))
    println(describe(-5))
    println(describe(200))
    println(describe("hello"))
    println(describe(List(1, 2, 3)))
    println(describe(Some(42)))
    println(describe(None))
  }
}

