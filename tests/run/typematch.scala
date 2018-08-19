object Test extends App {

  rewrite def f[T] = rewrite type T match {
    case String => println(s"String")
    case Int => println(s"Int")
  }

  f[String]
  f[Int]

}