enum Enum:
  case EC(val x: Int)

val a: Enum.EC { val x: 1 } = Enum.EC(1).asInstanceOf[Enum.EC { val x: 1 }]

import scala.language.experimental.modularity

enum EnumT:
  case EC(tracked val x: Int)

val b: EnumT.EC { val x: 1 } = EnumT.EC(1)

