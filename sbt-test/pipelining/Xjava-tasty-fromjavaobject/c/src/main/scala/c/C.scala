package c

import a.A

object C {

  val newA = new A

  @main
  def test = {
    newA.meth5(Array[Int](1)) // error: Array[Int] is not a subtype of Array[Object]
    newA.meth5_sel(Array[Int](1)) // error: Array[Int] is not a subtype of Array[Object]
    newA.meth6(Array[Int](1)) // error: Array[Int] is not a subtype of Array[T & Object]
  }
}

