@main def Test: Unit =
  inline def str1 = "Hello, "
  inline val str2 = "Scala 3"
  println(Str.concat(str1, str2))

  inline def i1 = 1
  inline val i2 = 2
  println(I.sum(i1, i2))