object PowerInlined1 {
  import Macro.Implementation._

  plus(0, 2)
  plus(1, 3)

  Macro.Implementation.plus(0, 2)
  Macro.Implementation.plus(1, 3)

  Macro.Implementation.Implementation2.plus(0, 2)
  Macro.Implementation.Implementation2.plus(1, 3)
}
