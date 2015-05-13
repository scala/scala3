trait Foo {
  @volatile private[this] var x: String = ""
  @volatile private var y: String = ""
}

class Bar extends Foo

object Test extends dotty.runtime.LegacyApp {
  classOf[Bar].getDeclaredFields.foreach(f => {
    assert(java.lang.reflect.Modifier.isVolatile(f.getModifiers), f.getName)
  })
}
