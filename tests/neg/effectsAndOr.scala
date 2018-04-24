
class Test {
  def fun1(b: Pure | Any) = ??? // error
  def fun2(b: Pure | Any | Any) = ??? // error
  def fun3(b: Any | Pure | Any) = ??? // error
  def fun4(b: Pure | Pure | Any) = ??? // error // error

  def fun5(b: Pure & Any) = ??? // error
  def fun6(b: Any & Pure & Any) = ??? // error
  def fun7(b: Pure & Any & Any) = ??? // error
  def fun8(b: Any & Any & Pure) = ??? // error
}
