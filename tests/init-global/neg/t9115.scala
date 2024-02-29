object D { // error
  def aaa = 1 //that’s the reason
  class Z (depends: Any)
  case object D1 extends Z(aaa) // 'null' when calling D.D1 first time  // error
  case object D2 extends Z(aaa) // 'null' when calling D.D2 first time
  println(D1)
  println(D2)
}
