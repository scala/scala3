package tests.signatureTestSource
class A
class B extends A
class C
class D[T]
class E[T] extends D[T]

class SignatureTestClass(a: String)
:
    def simple(): B
     = ???
    def oneParam(a: A): B
     = ???
    def multipleParams(a: A, b: B): C
     = ???
    def likeVararg(a: Seq[A]): C
     = ???
    def vararg(a: A*): C
     = ???
    def multipleList(a: A)(b: B): C
     = ???

    def generic[T](a: D[T]): D[T]
     = ???
    def generic2[T, V](a: D[T], b: E[V]): D[T]
     = ???

    def primitives(a: Int, b: Double, c: Short): Byte
     = 0
    def strings(a: String): String
     = ""
    def arrays(a: Array[String], b: Array[Int]): Array[Double]
     = ???
    def bounds1[T <: String](a: T, b: T): Unit
     = ???
    def bounds2[T >: String](a: T, b: T): Unit
     = ???
    def this()
     = this("Ala")