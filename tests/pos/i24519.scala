import scala.language.experimental.captureChecking

trait File extends caps.SharedCapability

class Foo:
  def f(file: File^): Iterator[File]^{file} = 
    Iterator.from(List(file))
