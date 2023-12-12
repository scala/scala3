package c

import a.AImport

object CImport {

  val newA = new AImport

  @main
  def test = {
    newA.meth5(Array[Int](1)) // error: Array[Int] is not a subtype of Array[Object]
  }
}

