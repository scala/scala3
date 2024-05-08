trait CmdLineParser { outer =>

  trait Opt[+T] {
    val default: T
    val names: Set[String]
    val help: String
  }

  trait IntOpt extends Opt[Int] {
    val parser = outer                          //       <=== comment out this line, we get "true true"
  }
}

object FirstParser extends CmdLineParser {
  object OptMinSuccess extends IntOpt {
    val default = 100
    val names = Set("bla")
    val help = "bla"
  }

  val opts = List(OptMinSuccess)
}

object SecondParser extends CmdLineParser {
  object OptMinSuccess extends IntOpt {
    val default = 50
    val names = Set("bla")
    val help = "help"
  }
}
@main def Test =

  val a = SecondParser.OptMinSuccess.isInstanceOf[FirstParser.IntOpt]

  println(a)

  (SecondParser.OptMinSuccess: SecondParser.IntOpt)  match {
    case _: FirstParser.IntOpt => println("true")
    case _ => println("false")
  }
