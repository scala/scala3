override trait T // error
override class X // error

package object p:
  override class Y // error

package p:
  trait Qlike:
    type Z

package p.q:
  object `package` extends p.Qlike:
    override class Z // error
