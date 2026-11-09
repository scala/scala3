// Compiles with published Scala 3.8.4 (via Vulpix `_c` suffix / Coursier).
// Repro for https://github.com/scala/scala3/issues/26626:
// macros that expand to Option.orNull must remain usable on newer compilers.

package sangria

import scala.quoted.*
import scala.compiletime.testing.typeChecks

object Macros:
  final val LegacyOrNullSnippet = "Option.empty[String].orNull[String | Null](using summon[Null <:< (String | Null)])"

  // To confirms it's Scala 3.8
  assert(typeChecks(LegacyOrNullSnippet))

  inline def useOrNull[T](inline o: Option[T]): T =
    ${ useOrNullImpl('o) }

  def useOrNullImpl[T: Type](o: Expr[Option[T]])(using Quotes): Expr[T] =
    '{ $o.orNull.asInstanceOf[T] }
