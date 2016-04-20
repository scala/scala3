package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.{InfoTransformer, SymTransformer}
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.{Flags, Names}
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** Move static methods from companion to the class itself */
class MoveStatics extends MiniPhaseTransform with SymTransformer { thisTransformer =>

  import tpd._
  override def phaseName = "moveStatic"


  def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation = {
    if (sym.hasAnnotation(defn.ScalaStaticAnnot) && sym.owner.is(Flags.Module)) {
      sym.owner.asClass.delete(sym.symbol)
      sym.owner.companionClass.asClass.enter(sym.symbol)
      val flags = if (sym.is(Flags.Method)) sym.flags else sym.flags | Flags.Mutable
      sym.copySymDenotation(owner = sym.owner.companionClass, initFlags = flags)
    }
    else sym
  }

  override def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] = {
    if (ctx.owner.is(Flags.Package)) {
      val (classes, others) = trees.partition(x => x.isInstanceOf[TypeDef] && x.symbol.isClass)
      val pairs = classes.groupBy(_.symbol.name.stripModuleClassSuffix).asInstanceOf[Map[Name, List[TypeDef]]]

      def move(companion: TypeDef, module: TypeDef): Thicket = {
        if (companion.symbol.is(Flags.Module)) move(module, companion)
        else {
          val allMembers = companion.rhs.asInstanceOf[Template].body ++ module.rhs.asInstanceOf[Template].body
          val (newCompanionBody, newModuleBody) = allMembers.partition(x => {assert(x.symbol.exists); x.symbol.owner == companion.symbol})
          def rebuild(orig: TypeDef, newBody: List[Tree]) = {
            val oldTemplate = orig.rhs.asInstanceOf[Template]
            val statics = newBody.filter(x => x.isInstanceOf[ValDef] && x.symbol.hasAnnotation(defn.ScalaStaticAnnot)).asInstanceOf[List[ValDef]]
            val newBodyWithStaticConstr =
              if (statics.nonEmpty) {
                val staticCostructor = ctx.newSymbol(orig.symbol, Names.STATIC_CONSTRUCTOR, Flags.Synthetic | Flags.JavaStatic | Flags.Method, MethodType(Nil, defn.UnitType))
                val staticAssigns = statics.map(x => Assign(ref(x.symbol), x.rhs.changeOwner(x.symbol, staticCostructor)))
                tpd.DefDef(staticCostructor, Block(staticAssigns, tpd.unitLiteral)) :: newBody
              } else newBody

            cpy.TypeDef(orig)(rhs = cpy.Template(orig.rhs)(oldTemplate.constr, oldTemplate.parents, oldTemplate.self, newBodyWithStaticConstr))
          }
          Thicket(rebuild(companion, newCompanionBody), rebuild(module, newModuleBody))
        }
      }
      val newPairs =
        for ((name, classes) <- pairs)
          yield
            if (classes.tail.isEmpty) classes.head
            else move(classes.head, classes.tail.head)
      Trees.flatten(newPairs.toList ++ others)
    } else trees
  }
}
