package dummy

trait BG {
  val   description: { type Structure }
  type  Structure =  description.Structure
}

abstract class Caller extends BG {
  type Foo >: this.type <: this.type

  transparent inline def generate2() =
    ${Macro.impl() }

  final val description = {
    generate2()
  }
}
