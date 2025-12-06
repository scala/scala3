import scala.annotation.targetName

class Foo @targetName("bla") () // error
