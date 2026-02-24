// All of type params shouldn't rename
class Foo[T]:
  def bar[T](x: T): T = x
  def baz[T](x: T, y: Foo[T]): T = x
  def qux[U](x: U, y: Foo[T]): U = x
  def quux[T, U](x: T, y: U, z: Foo[T]): (T, U) = (x, y)

// https://github.com/scala/scala3/issues/24671
final case class Bar[+T](t: T)

// `m: public <T1> T C.m()` rather than `m: public <T> T C.m()`
// that was wrong and could be crashed with
// `val c = C[String]; String x = c.<Object>m();`.
abstract class C[T]:
  def x: T
  def m[T] = x

// https://github.com/scala/scala3/issues/24134
// The mixin forwarders for compose in Function1 method  has signature
// `def compose[A](g: A => T1): A => R`
// Where the JavaPartialFunction[A, B] has type parameter A (name clash),
// The type parameter A in method should be renamed to avoid name duplication.
abstract class JavaPartialFunction[A, B] extends PartialFunction[A, B]

@main def Test(): Unit =
  val fooMethods = classOf[Foo[_]].getDeclaredMethods()
  printMethodSig(fooMethods, "bar")
  printMethodSig(fooMethods, "baz")
  printMethodSig(fooMethods, "qux")
  printMethodSig(fooMethods, "quux")

  val barMethods = classOf[Bar[_]].getDeclaredMethods()
  printMethodSig(barMethods, "copy")
  // copy$default$1 have `<T1> T Bar.copy$default$1` rather than `<T> T Bar.copy$default$1`
  // as reported in https://github.com/scala/scala3/issues/24671
  // The type parameter rename occurs because the return type T refers the enclosing class's type param T.
  printMethodSig(barMethods, "copy$default$1")

  val cMethods = classOf[C[_]].getDeclaredMethods()
  printMethodSig(cMethods, "m")

  val jpfMethods = classOf[JavaPartialFunction[_, _]].getDeclaredMethods()
  printMethodSigs(jpfMethods, "compose")

def printMethodSig(methods: Array[java.lang.reflect.Method], name: String): Unit =
  methods.find(_.getName.endsWith(name)).foreach { m =>
    println(s"$name: ${m.toGenericString}")
  }

def printMethodSigs(methods: Array[java.lang.reflect.Method], name: String): Unit =
  methods.filter(_.getName == name).sortBy(_.toGenericString).foreach { m =>
    println(s"$name: ${m.toGenericString}")
  }
