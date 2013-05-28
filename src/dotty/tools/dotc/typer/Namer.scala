package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._, Constants._, StdNames._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import util.Positions._
import util.SourcePosition
import language.implicitConversions

class Namer {

  import untpd._

  implicit def sourcePos(pos: Position)(implicit ctx: Context): SourcePosition =
    ctx.source.atPos(pos)

  implicit def posToCoord(pos: Position): Coord = positionCoord(pos)

  def privateWithinClass(mods: Modifiers)(implicit ctx: Context): Symbol = {
    val pw = mods.privateWithin
    if (pw.isEmpty) NoSymbol
    else {
      val cls = ctx.owner.enclosingClassNamed(pw)
      if (!cls.exists) ctx.error(s"no enclosing class or object is named $pw", mods.pos)
      cls
    }
  }

  def createSymbol(tree: Tree)(implicit ctx: Context): Symbol = tree match {
    case tree: ModDefTree =>
      val sym = ctx.newSymbol(
        ctx.owner, tree.name, tree.mods.flags, new Completer(tree),
        privateWithinClass(tree.mods), tree.pos)
      ctx.scope.enter(sym)
      sym
  }


  def expansion(tree: Tree)(implicit ctx: Context): Tree = {

    def expandCaseClass(tree: Tree, companion: Tree): Tree = {
      val ClassDef(mods, cname, tparams, impl @ Template(constr, parents, self, stats)) = tree
      val constr1 =
        if (constr.vparamss.nonEmpty) constr
        else {
          ctx.error("case classes need to have at least one parameter list")
          constr.derivedDefDef(constr.mods, constr.name, constr.tparams, ListOfNil, constr.tpt, constr.rhs)
        }
      val caseParams = constr1.vparamss.head
      val caseParamsArray = caseParams.toArray
      val isDefinedMeth = syntheticProperty(nme.isDefined, Literal(Constant(true)))
      val productArityMeth = syntheticProperty(nme.productArity, Literal(Constant(caseParamsArray.length)))
      val productElemMeths = for (i <- 0 until caseParamsArray.length) yield
          syntheticProperty(("_" + (i + 1)).toTermName, Select(This(EmptyTypeName), caseParamsArray(i).name))
      val clsTypeRef = AppliedTypeTree(Ident(cname), tparams map refOfDef)
      val methTparams = for (tparam <- tparams) yield
          tparam.derivedTypeDef(Modifiers(TypeParam), tparam.name, tparam.tparams, tparam.rhs)
      val (copyMeths, applyMeths) =
        if (mods is Abstract) (Nil, Nil)
        else {
          val creator = New(clsTypeRef, constr1.vparamss map (_ map refOfDef))
          val copyFirstParams = caseParams.map(vparam =>
            vparam.derivedValDef(Modifiers(TermParam), vparam.name, vparam.tpt, refOfDef(vparam)))
          val copyRestParamss = constr1.vparamss.tail.nestedMap(vparam =>
            vparam.derivedValDef(Modifiers(TermParam), vparam.name, vparam.tpt, EmptyTree))
          val applyParamss = constr1.vparamss.nestedMap(vparam =>
            vparam.derivedValDef(Modifiers(TermParam), vparam.name, vparam.tpt, vparam.rhs))
          val copyMeth =
            DefDef(Modifiers(Synthetic), nme.copy, methTparams, copyFirstParams :: copyRestParamss, EmptyTree, creator)
          val applyMeth =
            DefDef(Modifiers(Synthetoc), nme.apply, methTparams, applyParamss, EmptyTree, creator)
          (copyMeth :: Nil, applyMeth :: Nil)
      }
      val unapplyMeth = {
        val unapplyParam = makeSyntheticParameter(tpt = clsTypeRef)
        DefDef(Modifiers(Synthetic), nme.unapply, methTparams, (unapplyParam :: Nil) :: Nil, clsTypeRef, This(EmptyTypeName))
      }
      val newClassDefs = copyMeths ++ isDefinedMeth :: productArityMeth :: productElemMeths.toList
      val newModuleDefs = applyMeths ++ unapplyMeth :: Nil
      val cls1 = tree.derivedClassDef(mods, cname, tparams,
          impl.derivedTemplate(constr, parents, self, stats ++ newClassDefs))
      val companion1 = companion match {
        case ModuleDef(mods, name, impl @ Template(constr, parents, self, stats)) =>
          companion.derivedModuleDef(mods, name,
              impl.derivedTemplate(constr, parents, self, stats ++ newModuleDefs))
        case _ =>

      }
        if (companion.isEmpty)
        else {

        }

      val applyMeth = {
        val applyVparamss =
        DefDef(Modifiers(Synthetic), nme.apply, methTparams, applyVparamss, EmptyTree, )
      }

      }

  }


    tree match {
    case ValDef(mods, name, tpt, rhs) =>
      if (!ctx.owner.isClass || (mods is Private)) tree
      else {
        val lname = name.toLocalName
        val field = tree.derivedValDef(mods, lname, tpt, rhs)
        val getter = tree.derivedDefDef(mods, name, Nil, Nil, tpt, Ident(lname))
        if (!(mods is Mutable)) Thicket(field, getter)
        else {
          val setterParam = makeSyntheticParameter(tpt = TypeTree(field))
          val setter = tree.derivedDefDef(
            mods, name.getterToSetter, Nil, (setterParam :: Nil) :: Nil, EmptyTree, refOfDef(setterParam))
          Thicket(field, getter, setter)
        }
      }
    case tree: ModuleDef =>
      desugarModuleDef(tree)
    case tree: ClassDef if tree.mods is Case =>
      expandCaseClass(tree)
  }

  def syntheticProperty(name: TermName, rhs: Tree) = DefDef(Modifiers(Synthetic), name, Nil, Nil, EmptyTree, rhs)

  class Completer(tree: Tree) extends LazyType {
    def complete(sym: Symbol) =>
      ???
  }

  def enter(tree: Tree)(implicit ctx: Context) = tree match {
    case Import(expr, selectors) =>
      ???
    case DefDef


  }

}