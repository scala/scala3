package tests.inkuire

trait InType1
trait InType2 extends InType1

trait OutType1
trait OutType2 extends OutType1

class JustAClass {
  def mathod(l: InType1): OutType1 = ???
}

class JustAnotherClass extends JustAClass {
  def method(i: InType2): OutType2 = ???
}

object InkuireObject {
  def function(i: InType1): OutType1 = ???
  val value: InType1 => OutType1 = ???
}