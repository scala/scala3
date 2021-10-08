package example {

  class A1
  class A2
  class A3

  class TypeClass[A](val value: A)

  object TypeClass {
    def apply[A](implicit a: TypeClass[A]): TypeClass[A] = a

    def get2[X1: TypeClass, X2: TypeClass]: (X1, X2) = {
      (TypeClass[X1].value, TypeClass[X2].value)
    }

    def get3[X1: TypeClass, X2: TypeClass, X3: TypeClass]: (X1, X2, X3) = {
      (TypeClass[X1].value, TypeClass[X2].value, TypeClass[X3].value)
    }

    implicit def a1: TypeClass[A1] = new TypeClass[A1](new A1)
    implicit def a2: TypeClass[A2] = new TypeClass[A2](new A2)
    implicit def a3: TypeClass[A3] = new TypeClass[A3](new A3)
  }

  trait Foo {

    val (a2, a3) = TypeClass.get2[A2, A3]

    val (x1, x2, x3) = TypeClass.get3[A1, A2, A3]

  }

}

object Test {
  def main(args: Array[String]): Unit = {
    new example.Foo {}
  }
}