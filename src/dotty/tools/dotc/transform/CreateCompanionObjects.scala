package dotty.tools.dotc.transform

import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.core.{Scopes, Flags}
import dotty.tools.dotc.core.Symbols.NoSymbol
import scala.annotation.tailrec
import dotty.tools.dotc.core._
import Symbols._
import scala.Some
import dotty.tools.dotc.transform.TreeTransforms.{NXTransformations, TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import scala.collection.mutable
import dotty.tools.dotc.core.Names.Name
import NameOps._

/** A transformer that provides a convenient way to create companion objects
  */
abstract class CreateCompanionObjects extends TreeTransform {

  import tpd._

  /** Given class definition should return true if companion object creation should be enforced
    */
  def predicate(cls: TypeDef)(implicit ctx: Context): Boolean

  override def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[tpd.Tree] = {
    @tailrec
    def transformStats0(trees: List[Tree], acc: ListBuffer[Tree]): List[Tree] = {
      trees match {
        case Nil => acc.toList
        case (claz: TypeDef) :: stats if claz.symbol.isClass && !(claz.symbol is Flags.Module) => {
          val moduleExists = !(claz.symbol.companionModule eq NoSymbol)
          if (moduleExists || !predicate(claz)) transformStats0(stats, acc += claz)
          else {
            val moduleSymbol = ctx.newCompleteModuleSymbol(claz.symbol.owner, claz.name.toTermName, Flags.Synthetic, Flags.Synthetic, List(defn.ObjectClass.typeRef), Scopes.newScope)
            if (moduleSymbol.owner.isClass) moduleSymbol.entered
            val companion = tpd.ModuleDef(moduleSymbol, List(EmptyTree)).withPos(claz.pos)
            acc += claz
            acc += companion
            transformStats0(stats, acc)
          }
        }
        case stat :: stats => transformStats0(stats, acc += stat)
      }
    }

    transformStats0(trees, ListBuffer())
  }
}
