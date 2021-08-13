class Ptr[T](var value: T):
   def `unary_!` : T = value
   def `unary_!_=`(value: T): Unit = this.value = value
end Ptr

def test =
  val x = Ptr(9)
  !x = 10
  println(!x)
