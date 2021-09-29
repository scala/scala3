val x = 1
  + 2
  +3 // error: Expected a toplevel definition

val b1 = {
  22
  * 22  // ok
  */*one more*/22 // error: end of statement expected
}

val b2: Boolean = {
  println(x)
  ! "hello".isEmpty  // error: value ! is not a member of Unit
}

