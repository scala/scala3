import scala.compiletime.summonFrom
class Main(b: Boolean) {
  inline def foo(): Any = {
    summonFrom {
      case x => println(x) // error
    }

    summonFrom {
      case x if b => println(x) // error
    }

    summonFrom {
      case Some(x) if b => println(x) // error
    }

    summonFrom {
      case x if b => println(x) // error
      case _ => println()
    }

    summonFrom { // ok but useless
      case _ if b => println()
      case _ => println()
    }
  }

  foo()
}