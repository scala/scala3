package b

import a.A

// keep in sync with Bexplicit.scala
object B {

  val newA = new A

  val newAInner = new A.Inner[Int]()
  val newAInner_sel = new A.Inner_sel[Int]()

  @main
  def test = {
    newA.meth1(1) // OK
    newA.meth1_sel(1) // OK
    newA.meth2(1) // OK
    newA.meth3(List[Int](1)) // OK
    newA.meth3_sel(List[Int](1)) // OK
    newA.meth4(List[Int](1)) // OK
    newA.meth5(Array[Object]("abc")) // OK
    newA.meth5_sel(Array[Object]("abc")) // OK
    newA.meth6(Array[String]("abc")) // Ok
    // newA.meth5(Array[Int](1)) // error: Array[Int] is not a subtype of Array[Object]
    // newA.meth6(Array[Int](1)) // error: Array[Int] is not a subtype of Array[T & Object]
    newA.meth7(1) // OK (creates a reference array)
    newA.meth7_sel(1) // OK (creates a reference array)
    newA.meth8(1) // OK (creates a primitive array and copies it into a reference array at Erasure)
    val ai = Array[Int](1)
    newA.meth7(ai: _*) // OK (will copy the array at Erasure)
    newA.meth7_sel(ai: _*) // OK (will copy the array at Erasure)
    newA.meth8(ai: _*) // OK (will copy the array at Erasure)

    newAInner.meth1(1) // OK
    newAInner.meth2(1) // OK
    newAInner_sel.meth1(1) // OK
    newAInner_sel.meth2(1) // OK

    BImport.testImport() // OK
  }
}

