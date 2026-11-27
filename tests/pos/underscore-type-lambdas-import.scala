import scala.language.`3.12`

trait Foo[F[_]]
class Bar extends Foo[Either[Int, _]]
