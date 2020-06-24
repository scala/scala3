case class Cat(name: String, address: String = "Home", age: Int = 1)

@main def Test =
  println(defaultParams[Cat])
