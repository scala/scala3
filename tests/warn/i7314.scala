//> using options  -source future

@main def Test =
  // conversion out of the opaque type:
  val imm1 = IArray(1,2,3) // supposedly immutable
  println(imm1(0)) // 1
  imm1 match {
    case a: Array[Int] =>  // warn: should not be scrutinized
      a(0) = 0
  }
  println(imm1(0)) // 0
