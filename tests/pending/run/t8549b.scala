
@SerialVersionUID(42)
class C

@SerialVersionUID(43 - 1)
class D


object Test extends dotty.runtime.LegacyApp {
  def checkId(cls: Class[_]): Unit = {
    val id = cls.getDeclaredField("serialVersionUID").get(null)
    assert(id == 42, (cls, id))  
  }
  checkId(classOf[C])
  checkId(classOf[D])
}
