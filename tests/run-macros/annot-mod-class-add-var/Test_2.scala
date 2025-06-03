//> using options -experimental

@addCountToString("This is Foo: ")
class Foo:
  //> private var count$macro$1: Int = 0
  //> def toString(): String =
  //>   count$macro$1 = count$macro$1 + 1
  //>   "This is Foo" + count$macro$1

  var countA: Int = 0
  def toStringA(): String =
    countA = countA + 1
    "This is Foo" + countA


@addCountToString("This is Foo object: ")
object Foo
  //> private var count$macro$2: Int = 0
  //> def toString(): String =
  //>   count$macro$2 = count$macro$2 + 1
  //>   "This is Foo object: " + count$macro$2

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.toString() == "This is Foo: 1", foo)
  assert(foo.toString() == "This is Foo: 2", foo)
  assert(Foo.toString() == "This is Foo object: 1", Foo)
  assert(Foo.toString() == "This is Foo object: 2", Foo)
