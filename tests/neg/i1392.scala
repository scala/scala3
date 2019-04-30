class A { def m = 1 }
class B extends A { override def m = 2 }
trait T extends A
class C extends B with T {
  override def m = super[T].m // error: Super call cannot be emitted: the selected method m is declared in class A, which is not the direct superclass of class C.
}
