class X(val y: String)
class Y(y: => String) extends X(y)
class Z(z: => String) extends X(z)
