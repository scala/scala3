object Test extends App {

  class TC {
    println("TC")
  }

  class TC1

  given as TC1

  class TV(val tc: TC) extends AnyVal

  trait C {
    val x: TC
    given as TC = x
    summon[TC]
    summon[TC]
  }
  class D extends C {
    override val x = new TC
  }

  locally{
    println("= new")
    given t as TC = new TC
    summon[TC]
    summon[TC]
  }

  locally {
    println("= x")
    new D
  }

  locally{
    println("= new VC")
    given t as TV = new TV(new TC)
    summon[TV]
    summon[TV]
  }

  class TCC {
    val tc: TC = new TC
  }

  val tcc = new TCC
  locally {
    println("= x.y")
    given t as TC = tcc.tc
    summon[TC]
    summon[TC]
  }

  locally {
    println("with given")
    given t as TC given TC1 = new TC
    summon[TC]
    summon[TC]
  }

  locally {
    println("with type params")
    given t[X] as TC = new TC
    summon[TC]
    summon[TC]
  }
}