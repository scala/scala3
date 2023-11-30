import scala.annotation.targetName

def doClose(closable: { def close(): Unit }): Unit =
  import reflect.Selectable.reflectiveSelectable
  closable.close()

class Resource:
  @targetName("foo")
  def close(): Unit = ???

def test = doClose(Resource()) // error