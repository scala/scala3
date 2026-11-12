package lib

object Api {
  // The pickle shares one constant entry between the `ConstantType` of `Msg`
  // and the argument of `@deprecated`.
  final val Msg = "msg"
  @deprecated("msg", "1.0") def old(): Int = 1
}
