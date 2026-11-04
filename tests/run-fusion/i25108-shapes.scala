// The ways a trait val can be overridden in a class that implements the trait's setter.
// None of them may let the setter write the overriding val's field.

trait Logger:
  val prefix: String = "[log]"
  val seenInInit: String = prefix

class Service extends Logger:
  override val prefix: String = "[svc]"

trait Override extends Logger:
  override val prefix: String = "[override]"

class Linearized extends Logger with Override

class Param(override val prefix: String) extends Logger

class Plain extends Logger

// The real-world use case noted on the ticket, slick-pg's ExPostgresProfile: each trait in the
// chain narrows the type of a concrete val.
trait RelationalTableComponent:
  class RelationalColumnOptions(val tag: String)
  val columnOptions: RelationalColumnOptions = RelationalColumnOptions("relational")
  val seenInInit: String = if columnOptions == null then "null" else columnOptions.tag

trait SqlTableComponent extends RelationalTableComponent:
  class SqlColumnOptions(tag: String) extends RelationalColumnOptions(tag)
  override val columnOptions: SqlColumnOptions = SqlColumnOptions("sql")

trait ExPostgresProfile extends SqlTableComponent:
  class ColumnOptions(tag: String) extends SqlColumnOptions(tag)
  override val columnOptions: ColumnOptions = ColumnOptions("pg")

// The typical usage pattern is fine: the profile inherits the narrowed val, so its getter is a
// Mixin forwarder, created after the traits' setters were transformed, and those stayed `()`.
object MyPostgresProfile extends ExPostgresProfile

// However, overriding the val once more in the object made the first trait's setter write
// the trait's value, cast to the narrowest type, into the object's field: ClassCastException.
object MyPostgresProfile2 extends ExPostgresProfile:
  override val columnOptions: ColumnOptions = ColumnOptions("own")

@main def Test =
  for l <- List(Service(), Linearized(), Param("[param]"), Plain()) do
    println((l.prefix, l.seenInInit))
  for p <- List(MyPostgresProfile, MyPostgresProfile2) do
    println((p.columnOptions.tag, p.seenInInit))
