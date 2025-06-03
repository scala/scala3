trait Ops { self: MyCodes =>
  abstract class Instru
  object opcodes {
    case class SWITCH(i:Int) extends Instru
    case object EmptyInstr extends Instru
  }
}

trait Blox { self: MyCodes =>
  import opcodes.*
  class Basick {
    var foo: Instru = null

    def bar = foo match {
      case SWITCH(i) => i
      case EmptyInstr => 0
    }
  }
}

class MyCodes extends AnyRef with Ops with Blox {
  val a = b
  val b: Int = 10      // warn
}
