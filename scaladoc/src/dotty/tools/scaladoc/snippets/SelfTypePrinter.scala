package dotty.tools.scaladoc
package snippets

import dotty.tools.dotc.printing.RefinedPrinter
import dotty.tools.dotc.core._
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.TypeErasure.ErasedValueType
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Denotations._
import dotty.tools.dotc.core.SymDenotations._
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.typer.{Implicits, Namer, Applications}
import dotty.tools.dotc.typer.ProtoTypes._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.TypeApplications._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.util.Chars.isOperatorPart
import dotty.tools.dotc.transform.TypeUtils._
import dotty.tools.dotc.transform.SymUtils._

import language.implicitConversions
import dotty.tools.dotc.util.{NameTransformer, SourcePosition}
import dotty.tools.dotc.ast.untpd.{MemberDef, Modifiers, PackageDef, RefTree, Template, TypeDef, ValOrDefDef}

class SelfTypePrinter(using _ctx: Context) extends RefinedPrinter(_ctx):

  private def refinementChain(tp: Type): List[Type] =
    tp :: (tp match {
      case tp: RefinedType => refinementChain(tp.parent.stripTypeVar)
      case _ => Nil
    })

  override def toText(tp: Type): Text = tp match
    case tp: RefinedType =>
      val parent :: (refined: List[RefinedType @unchecked]) =
        refinementChain(tp).reverse
      toTextLocal(parent)
    case tp => super.toText(tp)

  override def toTextSingleton(tp: SingletonType): Text =
      tp match
        case ConstantType(value) =>
          if value.tag == Constants.ByteTag || value.tag == Constants.ShortTag then
            toText(value) ~ s" /*${value.tpe.show}*/"
          else
            toText(value)
        case _: TermRef => toTextRef(tp) ~ ".type /*" ~ toTextGlobal(tp.underlying) ~ "*/"
        case _ => "(" ~ toTextRef(tp) ~ ": " ~ toTextGlobal(tp.underlying) ~ ")"
