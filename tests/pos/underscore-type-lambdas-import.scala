import scala.language.`3.11`

trait Foo[F[_]]
class Bar extends Foo[Either[Int, _]]
