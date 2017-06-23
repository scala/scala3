import language.strictEquality
object equality1 {
  class A
  class B
  new A == new B // error: cannot compare
}
