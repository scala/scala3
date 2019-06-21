object Test extends App {

  class TC {
    println("TC")
  }

  class TC1
  delegate for TC1

  class TV(val tc: TC) extends AnyVal

  trait C {
    val x: TC
    delegate for TC = x
    the[TC]
    the[TC]
  }
  class D extends C {
    override val x = new TC
  }

  locally{
    println("= new")
    delegate t for TC = new TC
    the[TC]
    the[TC]
  }

  locally {
    println("= x")
    new D
  }

  locally{
    println("= new VC")
    delegate t for TV = new TV(new TC)
    the[TV]
    the[TV]
  }

  class TCC {
    val tc: TC = new TC
  }

  val tcc = new TCC
  locally {
    println("= x.y")
    delegate t for TC = tcc.tc
    the[TC]
    the[TC]
  }

  locally {
    println("with given")
    delegate t for TC given TC1 = new TC
    the[TC]
    the[TC]
  }

  locally {
    println("with type params")
    delegate t[X] for TC = new TC
    the[TC]
    the[TC]
  }
}