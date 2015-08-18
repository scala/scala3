object Test {

  def takesPartialFunction(a: PartialFunction[Int, Int]) = a(1) 

  def main(args: Array[String]): Unit = {
    val partialFunction: PartialFunction[Int, Int] = {case a: Int => a}

    assert(takesPartialFunction(partialFunction) == 1)
  }
}
