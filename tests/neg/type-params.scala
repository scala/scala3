
def `t8219 Any == is not overloaded in error message`: Unit =
  // Scala 2 once defined AnyRef == and Any == as overloaded. Spec defines only Any ==.
  "".==[Int] // error
  ("": AnyRef).==[Int] // error
  ("": Object).==[Int] // error

def `extra type arg to class literal` =
  classOf[Int][Int] // error

class `various ancient kindedness`:

  class WellKinded[x]:
    type t = x[x] // error
  trait WellKindedBounded[s <: Throwable]:
    val foo: s[Int] // error
  class WellKindedWrongSyntax[s <: List] // must be s[x] <: List[x] ?

  class Monad[m[x]]

  object mp extends Monad[Tuple2] // error

  // expecting types of kind *->*
  trait ms1 extends Monad[String]        // error
  trait ms2[t] extends Monad[t]          // error
  trait ms3[m[_], t] extends Monad[m[t]] // error -- added to check regression on bug

  // expecting types of kind *
  trait Foo[x]
  trait Bar1[m[_]] extends Foo[m[Int]] // check that m[Int] is properly recognized as kind-*
  trait Bar2[m[_]] extends Foo[m] // error check that m is properly recognized as kind *->*, while * is expected

def `t2918 t5093 detect cyclic error` =
  def g[X, A[X] <: A[X]](x: A[X]) = x // error // error
  def f[C[X] <: C[X]](l: C[_]) = l.x // error // error

class t1701 extends java.lang.Cloneable[String, Option, Int] // error

trait t0842[T] { def m: this.type[T] = this } // error

trait t278:
  class A
  def a = () => ()
  def a = (p: A) => () // error
  println(a[A]) // error
