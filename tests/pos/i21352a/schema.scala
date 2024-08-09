//> using options -source:3.5

case class Schema[T](format: String):
  def asOption: Schema[Option[T]] = ???
  def name(name: Option[SName]): Schema[T] = ???
  def format(f: String): Schema[T] = ???

object Schema extends SchemaCompanionMacros:
  implicit def schemaForOption[T: Schema]: Schema[Option[T]] =
    implicitly[Schema[T]]
    ???

trait SchemaCompanionMacros extends SchemaDerivation:
  given derivedStringBasedUnionEnumeration[S](using IsUnionOf[String, S]): Schema[S] =
    val x: Schema[S] = ???
    x.name(None)

@main def Test =
  case class Foo(x: Int) derives Schema
