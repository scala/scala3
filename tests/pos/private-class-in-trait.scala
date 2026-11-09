trait T {
  private val classValue = new PrivateClass
  private val objectValue = PrivateObject

  private val classOption: Option[PrivateClass] = Some(new PrivateClass)
  private val objectOption: Option[PrivateObject.type] = Some(PrivateObject)

  private class PrivateClass
  private object PrivateObject
}

object Test extends T
