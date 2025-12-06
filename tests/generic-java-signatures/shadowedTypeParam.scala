// All of type params shouldn't rename
class Foo[T]:
  def bar[T](x: T): T = x
  def baz[T](x: T, y: Foo[T]): T = x
  def qux[U](x: U, y: Foo[T]): U = x
  def quux[T, U](x: T, y: U, z: Foo[T]): (T, U) = (x, y)

// https://github.com/scala/scala3/issues/24671
final case class Bar[+T](t: T)

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
  // copy$default$1 still have `<T1> T Bar.copy$default$1`
  // but it should be `Bar<T> Bar.copy$default$1`
  // (It shouldn't have unused type param, and return type should be Bar<T>)

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
