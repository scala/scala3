import Specializable._

object specializable_specialization {
  def foo[@specialized(Primitives) T](t: T): T = t
  def foo2[@specialized(Everything) T](t: T): T = t
  def foo3[@specialized(Bits32AndUp) T](t: T): T = t
  def foo4[@specialized(Integral) T](t: T): T = t
  def foo5[@specialized(AllNumeric) T](t: T): T = t
  def foo6[@specialized(BestOfBreed) T](t: T): T = t

  def main(args: Array[String]) = {
    foo('c')
    foo5('c')
  }
}
