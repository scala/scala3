import scala.compiletime.*

trait A:
  type F
  type Q = F

trait Reader[-In, Out]
object Reader:
  given [X]: Reader[A { type Q = X }, X] with {}

class Test:
  //type BC = A { type F = Int } & A // ok
  type BC = A & A { type F = Int } // fail, also ok when manually de-aliased

  inline def summonOne: Unit = summonInline[Reader[BC, Int]]

  def t1(): Unit = summonInline[Reader[BC, Int]] // ok
  def t2(): Unit = summonOne // error
