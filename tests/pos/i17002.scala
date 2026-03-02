import scala.annotation.meta.companionMethod

@companionMethod
class methOnly extends annotation.Annotation

class Test
object Test:

  @methOnly
  given test2: [T] => Test()
