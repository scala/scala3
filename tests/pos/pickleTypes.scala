object pickleTypes {

  abstract class C { type T; val x: T; def f: T; def g(y: T): T; def h[U](z: U, y: T): U }

  val x1: Int = ???
  val x2: List[List[Int]] = ???
  val x3: C { type T <: C } = ???
  val x4: C { type T = Int; val x: Int } = ???
  val x5: C { type T = String; def f: String; def g(y: String): String } = ???
  val x6: C { type T = String; def f: String; def g(y: String): String; def h[U](z: U, y: T): U } = ???


}
