package b

import a.AImport

object BImport {

  val newA = new AImport

  val newAInner = new AImport.Inner[Int]()

  def testImport() = {
    newA.meth1(1) // OK
    newA.meth3(List[Int](1)) // OK
    newA.meth5(Array[Object]("abc")) // OK
    // newA.meth5(Array[Int](1)) // error: Array[Int] is not a subtype of Array[Object]
    newA.meth7(1) // OK (creates a reference array)
    val ai = Array[Int](1)
    newA.meth7(ai: _*) // OK (will copy the array at Erasure)

    newAInner.meth1(1) // OK
  }
}

