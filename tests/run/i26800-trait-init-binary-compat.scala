// scalajs: --skip

trait WithNestedObject: // see #26800
  object Nested

trait WithVal:
  val x = 1

trait WithStatement:
  println("init")

trait WithDef:
  def f = 1

trait WithAbstractDef:
  def f: Int

// Traits that need initialization must emit `$init$` for binary compatibility (#26800).
object Test:
  def main(args: Array[String]): Unit =
    def hasInit(cls: Class[?]) =
      cls.getMethods.map(_.toString).exists(_.contains("$init$"))
    assert(!hasInit(classOf[WithDef]))
    assert(!hasInit(classOf[WithAbstractDef]))
    // traits with object/val/statement are marked with Init
    assert(hasInit(classOf[WithNestedObject]))
    assert(hasInit(classOf[WithVal]))
    assert(hasInit(classOf[WithStatement]))
