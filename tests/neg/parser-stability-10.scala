object i0 {
def unapply(i1: Int)(i6: List[Int]): Int = {
  def i7(i8: i0) = i1 match {  // error
    case i0(_) =>              // error
      (i1, i8) match {
        case i0(i1, i1) => case _ => i2  // error // error
      }
  }
}  // error
object i5 {
  import collection.mutable._
  try { ??? mutable { case i1(i5, i3, i4) => i5 }}  // error
}
// error