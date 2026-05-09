// https://github.com/scala/scala3/issues/25943
class C25943(val a: AnyRef)

def runIt(): Unit =
  val a2 = new Object

  val obj =
    new :
      class S extends C25943(a2)
      val s = new S{}

  val sField = obj.getClass.getDeclaredField("s")
  sField.setAccessible(true)
  val sInstance = sField.get(obj).asInstanceOf[C25943]
  assert(sInstance.a eq a2, s"expected $a2, got ${sInstance.a}")

@main def Test = runIt()
