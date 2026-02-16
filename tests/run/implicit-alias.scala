object Test extends App {

  class TC {
    println("TC")
  }

  class TC1

  given TC1()

  class TV(val tc: TC) extends AnyVal

  trait C {
    val x: TC
    given TC = x
    summon[TC]
    summon[TC]
  }
  class D extends C {
    override val x = new TC
  }

  locally{
    println("= new")
    given t: TC = new TC
    summon[TC]
    summon[TC]
  }

  locally {
    println("= x")
    new D
  }

  locally{
    println("= new VC")
    given t: TV = new TV(new TC)
    summon[TV]
    summon[TV]
  }

  class TCC {
    val tc: TC = new TC
  }

  val tcc = new TCC
  locally {
    println("= x.y")
    given t: TC = tcc.tc
    summon[TC]
    summon[TC]
  }

  locally {
    println("with given")
    given t(using TC1): TC  = new TC
    summon[TC]
    summon[TC]
  }

  locally {
    println("with type params")
    given t: [X] => TC = new TC
    summon[TC]
    summon[TC]
  }
}