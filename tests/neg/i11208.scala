import scala.reflect.ClassTag

@main def run = println(Foo)

abstract class Bar[T](implicit val thisClassTag: ClassTag[T])

class Foo
object Foo extends Bar[Foo] // error