//> using options -Werror -deprecation -feature

@main
def Test =

  val x: Int | String = 1

  println(x.isInstanceOf[Int])

  x match
    case _: Int =>    println("Int")
    case _: String => println("String")
