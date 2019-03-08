package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

object MoveStatics {
  val name: String = "moveStatic"
}

/** Move static methods from companion to the class itself */
class MoveStatics extends MiniPhase with SymTransformer {

  import tpd._
  override def phaseName: String = MoveStatics.name

  def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation = {
    if (sym.hasAnnotation(defn.ScalaStaticAnnot) && sym.owner.is(Flags.Module) && sym.owner.companionClass.exists &&
        (sym.is(Flags.Method) || !(sym.is(Flags.Mutable) && sym.owner.companionClass.is(Flags.Trait)))) {
      sym.owner.asClass.delete(sym.symbol)
      sym.owner.companionClass.asClass.enter(sym.symbol)
      sym.copySymDenotation(owner = sym.owner.companionClass)
    }
    else sym
  }

  override def transformStats(trees: List[Tree])(implicit ctx: Context): List[Tree] = {
    if (ctx.owner.is(Flags.Package)) {
      val (classes, others) = trees.partition(x => x.isInstanceOf[TypeDef] && x.symbol.isClass)
      val pairs = classes.groupBy(_.symbol.name.stripModuleClassSuffix).asInstanceOf[Map[Name, List[TypeDef]]]

      def rebuild(orig: TypeDef, newBody: List[Tree]): Tree = {
        if (orig eq null) return EmptyTree

        val staticFields = newBody.filter(x => x.isInstanceOf[ValDef] && x.symbol.hasAnnotation(defn.ScalaStaticAnnot)).asInstanceOf[List[ValDef]]
        val newBodyWithStaticConstr =
          if (staticFields.nonEmpty) {
            /* do NOT put Flags.JavaStatic here. It breaks .enclosingClass */
            val staticCostructor = ctx.newSymbol(orig.symbol, nme.STATIC_CONSTRUCTOR, Flags.Synthetic | Flags.Method | Flags.Private, MethodType(Nil, defn.UnitType))
            staticCostructor.addAnnotation(Annotation(defn.ScalaStaticAnnot))
            staticCostructor.entered

            val staticAssigns = staticFields.map(x => Assign(ref(x.symbol), x.rhs.changeOwner(x.symbol, staticCostructor)))
            tpd.DefDef(staticCostructor, Block(staticAssigns, tpd.unitLiteral)) :: newBody
          } else newBody

        val oldTemplate = orig.rhs.asInstanceOf[Template]
        cpy.TypeDef(orig)(rhs = cpy.Template(oldTemplate)(body = newBodyWithStaticConstr))
      }

      def move(module: TypeDef, companion: TypeDef): List[Tree] = {
        assert(companion != module)
        if (!module.symbol.is(Flags.Module)) move(companion, module)
        else {
          val allMembers =
            (if(companion != null) {companion.rhs.asInstanceOf[Template].body} else Nil) ++
            module.rhs.asInstanceOf[Template].body
          val (newModuleBody, newCompanionBody) = allMembers.partition(x => {assert(x.symbol.exists); x.symbol.owner == module.symbol})
          Trees.flatten(rebuild(companion, newCompanionBody) :: rebuild(module, newModuleBody) :: Nil)
        }
      }
      val newPairs =
        for ((name, classes) <- pairs)
          yield
            if (classes.tail.isEmpty)
              if (classes.head.symbol.is(Flags.Module)) move(classes.head, null)
              else List(rebuild(classes.head, classes.head.rhs.asInstanceOf[Template].body))
            else move(classes.head, classes.tail.head)
      Trees.flatten(newPairs.toList.flatten ++ others)
    } else trees
  }
}
