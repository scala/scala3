trait A(a: Any, b: Int) {
  //var x = 0
}
//  class A(a: String, b: Int) {
//
//  }                                           //OK!
  object B extends A(b = 0, a = String(""))
//  object B extends A(a = String(""), b = 0)   //OK!