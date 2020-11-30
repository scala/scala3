object patternUnsoundness extends App {

  class C[+T]

  case class D[S](_s: S) extends C[S] {
    var s: S = _s
  }

  val x = new D[String]("abc")
  val y: C[Object] = x

  y match {
    case d @ D(x) => d.s = new Integer(1) // error
  }

  val z: String = x.s // used to throw ClassCast exception
}
