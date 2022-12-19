@main def Test() =
  println(showToString[1])
  println(showToString["abc"])
  println(showToString[Int])
  println(showToString[List[Int]])
  println(showToString[Object { def foo: Int }])
