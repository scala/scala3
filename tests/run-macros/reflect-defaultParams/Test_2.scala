case class Cat(name: String, address: String = "Home", age: Int = 1)(a: Int = age, b: String = address + age)

@main def Test =
  println(defaultParams[Cat])
