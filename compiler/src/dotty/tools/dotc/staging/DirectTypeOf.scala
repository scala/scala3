package dotty.tools.dotc.staging

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

object DirectTypeOf:
  import tpd.*

  /** Matches `x.Underlying` and extracts the TermRef to `x` */
  def unapply(tpe: Type)(using Context): Option[TermRef] = tpe match
    case tp @ TypeRef(x: TermRef, _) if tp.symbol == defn.QuotedType_splice => Some(x)
    case _ => None

  object Healed:
    /** Matches `{ @SplicedType type T = x.Underlying; T }` and extracts the TermRef to `x` */
    def unapply(body: Tree)(using Context): Option[TermRef] =
      body match
        case Block(List(tdef: TypeDef), tpt: TypeTree) =>
          tpt.tpe match
            case tpe: TypeRef if tpe.typeSymbol == tdef.symbol =>
              DirectTypeOf.unapply(tdef.rhs.tpe.hiBound)
            case _ => None
        case _ => None
