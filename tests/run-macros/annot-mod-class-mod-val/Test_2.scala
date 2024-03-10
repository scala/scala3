//> using options -experimental -Yno-experimental

@setValue("valDef", "a")
@setValue("varDef", "b")
@setValue("lazyVarDef", "c")
class Foo:
  val valDef: String = "?" //> val valDef: String = "a"
  var varDef: String = "?" //> var varDef: String = "b"
  lazy val lazyVarDef: String = "?" //> lazy val lazyVarDef: String = "c"

@setValue("valDef", "a")
@setValue("varDef", "b")
@setValue("lazyVarDef", "c")
object Foo:
  val valDef: String = "?" //> val valDef: String = "a"
  var varDef: String = "?" //> var varDef: String = "b"
  lazy val lazyVarDef: String = "?" //> lazy val lazyVarDef: String = "c"

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.valDef == "a", foo.valDef)
  assert(foo.varDef == "b", foo.varDef)
  assert(foo.lazyVarDef == "c", foo.lazyVarDef)

  assert(Foo.valDef == "a", Foo.valDef)
  assert(Foo.varDef == "b", Foo.varDef)
  assert(Foo.lazyVarDef == "c", Foo.lazyVarDef)
