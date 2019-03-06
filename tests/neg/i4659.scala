class A(val a: A)(val b:a.T) {
  type T
}

object a0 extends A(a0)(0) {  // error
  type T = Int
}

object Test extends App {
  new A(a0)(1)
}