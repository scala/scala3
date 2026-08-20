class A { def m = 1 }
class B extends A { override def m = 2 }
trait T extends A
class C extends B with T {
  override def m = super[T].m // error // should invoke A.m
}
