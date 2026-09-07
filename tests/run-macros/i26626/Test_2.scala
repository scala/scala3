//> using options -Yexplicit-nulls
import scala.compiletime.testing.typeChecks

object AstVisitor:
  def visit(o: Option[String]): String = sangria.Macros.useOrNull(o)

@main def Test =
  assert(AstVisitor.visit(Some("ok")) == "ok")
  assert(AstVisitor.visit(None) == null)

  // protected/private[scala] def orNull[A1 >: A](implicit ev: Null <:< A1): A1
  // typechecks under 3.8, tested in Macro
  assert(!typeChecks(sangria.Macros.LegacyOrNullSnippet))
  val _: String | Null = Option.empty[String].orNull
