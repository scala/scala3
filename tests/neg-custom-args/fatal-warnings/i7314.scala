object Test {

  // conversion into the opaque type:
  val arr = Array(1,2,3)
  val imm0: IArray[Int] // supposedly immutable
    = oops(arr)
  println(imm0(0)) // 1
  arr(0) = 0
  println(imm0(0)) // 0, the value has changed!

  // conversion out of the opaque type:
  val imm1 = IArray(1,2,3) // supposedly immutable
  println(imm1(0)) // 1
  imm1 match {
    case a: Array[Int] =>   // error: type test of scrutinee of opaque type opaques.IArray[Int] against type Array[Int] cannot be checked at runtime
      a(0) = 0
  }
  println(imm1(0)) // 0

  opaque type IA[A] = Array[A]

  ??? : IA[Int] match {
    case a: IArray[Int] => a  // error: type test of scrutinee of opaque type Test.IA[Int] against opaque type opaques.IArray[Int] cannot be checked at runtime
  }

  def oops(a: Array[Int]): IArray[Int] = a match {
    case a: IArray[Int] => a // error: type test of scrutinee of type Array[Int] against opaque type opaques.IArray[Int] cannot be checked at runtime
  }
}