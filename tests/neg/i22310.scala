//> using options -Xfatal-warnings

// This should fail compilation but currently passes
abstract class A0[T] {
  def func(arg0: A0[String], arg1: T): Unit
}

abstract class A1 extends A0[String] {
  override def func(arg0: A0[Object], arg1: String): Unit = {} // error: overrides nothing
}

// Similar issue with multiple type parameters
abstract class B0[T, U] {
  def method(x: B0[String, Int], y: T, z: U): Unit
}

abstract class B1 extends B0[String, Int] {
  override def method(x: B0[Object, Int], y: String, z: Int): Unit = {} // error: overrides nothing
}

// This should work (correct override)
abstract class C0[T] {
  def goodFunc(arg0: C0[String], arg1: T): Unit
}

abstract class C1 extends C0[String] {
  override def goodFunc(arg0: C0[String], arg1: String): Unit = {} // OK - this should work
}