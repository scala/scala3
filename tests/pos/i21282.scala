//> using options -Werror -source:future-migration

import scala.quoted.*

private def isUnionCanonicalImpl[U: Type](using Quotes): Expr[Unit] =
  import quotes.reflect.*
  val u = TypeRepr.of[U].dealiasKeepOpaques

  def inner[U: Type](s: Set[TypeRepr], tr: TypeRepr): Set[TypeRepr] =
    tr.dealiasKeepOpaques match
      case OrType(a, b) =>
        val ss = inner[U](s, a)
        inner[U](ss, b)
      case x if s.contains(x) =>
        report.errorAndAbort(s"Type ${x.show} multiple times (CHECK ALIASES) in union ${u.show}")
      case x => s + x
  inner(Set.empty, u)
  '{ () }
