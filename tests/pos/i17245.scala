import scala.reflect.ClassTag

trait MockSettings

object Mockito {
  def mock[T : ClassTag]: T = ???
  def mock[T : ClassTag](settings: MockSettings): T = ???
}

trait Channel
type OnChannel = Channel => Any

@main def Test =
  val case1: OnChannel = Mockito.mock[OnChannel]
  val case2: OnChannel = Mockito.mock
  val case3 = Mockito.mock[OnChannel]
  val case4: OnChannel =  Mockito.mock[OnChannel](using summon[ClassTag[OnChannel]])

  // not a regressive case, but an added improvement with the fix for the above
  val case5: Channel => Any = Mockito.mock[Channel => Any]
