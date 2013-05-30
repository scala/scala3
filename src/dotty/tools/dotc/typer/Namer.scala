package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._, Constants._, StdNames._, Scopes._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import util.Positions._
import util.SourcePosition
import collection.mutable
import language.implicitConversions

trait NamerContextOps { ctx: Context =>

  def enterSym(sym: Symbol) = ctx.owner match {
    case cls: ClassSymbol => cls.enter(sym)
    case _ => this.scope.asInstanceOf[MutableScope].enter(sym)
  }
}

class Namer(val typer: Typer) {

  import untpd._

  val expandedTrees = new mutable.WeakHashMap[Tree, Tree]()
  val symOfTree = mutable.Map[Tree, Symbol]()

  implicit def sourcePos(pos: Position)(implicit ctx: Context): SourcePosition =
    ctx.source.atPos(pos)

  implicit def posToCoord(pos: Position): Coord = positionCoord(pos)

  def enclosingStats(implicit ctx: Context): List[Trees.Tree[_ >: Untyped]] =
    if (ctx == NoContext) Nil
    else ctx.tree match {
      case Template(_, _, _, stats) => stats
      case Block(stats, _) => stats
      case PackageDef(_, stats) => stats
      case _ => enclosingStats(ctx.outer)
    }

  def privateWithinClass(mods: Modifiers)(implicit ctx: Context): Symbol = {
    val pw = mods.privateWithin
    if (pw.isEmpty) NoSymbol
    else {
      val cls = ctx.owner.enclosingClassNamed(pw)
      if (!cls.exists) ctx.error(s"no enclosing class or object is named $pw", mods.pos)
      cls
    }
  }

  def createSymbol(tree: Tree, original: Tree)(implicit ctx: Context): Symbol = {
    def createSym(name: Name, flags: FlagSet, privateWithin: Symbol) = {
      val sym = ctx.newSymbol(ctx.owner, name, flags, new Completer(original), privateWithin, original.pos)
      symOfTree(original) = sym
      sym
    }
    tree match {
      case tree: ModDefTree =>
        val sym = createSym(tree.name, tree.mods.flags, privateWithinClass(tree.mods))
        ctx.enterSym(sym)
        sym
      case imp: Import =>
        createSym(nme.IMPORT, Synthetic, NoSymbol)
      case _ =>
        NoSymbol
    }
  }

  val synthetic = Modifiers(Synthetic)

  def expansion(tree: Tree)(implicit ctx: Context): Tree = {

    def classTypeRef(cdef: ClassDef) = {
      val tycon = Ident(cdef.name)
      if (cdef.tparams.isEmpty) tycon else AppliedTypeTree(tycon, cdef.tparams map refOfDef)
    }

    def creator(cdef: ClassDef) =
      New(classTypeRef(cdef), cdef.impl.constr.vparamss.nestedMap(refOfDef))

    def methTypeParams(cdef: ClassDef) =
      for (tparam <- cdef.tparams) yield
        tparam.derivedTypeDef(Modifiers(TypeParam), tparam.name, tparam.tparams, tparam.rhs)

    def methParamss(cdef: ClassDef) =
      cdef.impl.constr.vparamss.nestedMap(vparam =>
        vparam.derivedValDef(Modifiers(TermParam), vparam.name, vparam.tpt, vparam.rhs))

    def expandCaseClass(cdef: ClassDef): ClassDef = {
      val ClassDef(mods, cname, tparams, impl @ Template(constr, parents, self, stats)) = cdef
      val constr1 =
        if (constr.vparamss.nonEmpty) constr
        else {
          ctx.error("case class needs to have at least one parameter list", cdef.pos)
          constr.derivedDefDef(constr.mods, constr.name, constr.tparams, ListOfNil, constr.tpt, constr.rhs)
        }
      val caseParams = constr1.vparamss.head
      val caseParamsArray = caseParams.toArray
      def syntheticProperty(name: TermName, rhs: Tree) = DefDef(synthetic, name, Nil, Nil, EmptyTree, rhs)
      val isDefinedMeth = syntheticProperty(nme.isDefined, Literal(Constant(true)))
      val productArityMeth = syntheticProperty(nme.productArity, Literal(Constant(caseParamsArray.length)))
      val productElemMeths = for (i <- 0 until caseParamsArray.length) yield
          syntheticProperty(("_" + (i + 1)).toTermName, Select(This(EmptyTypeName), caseParamsArray(i).name))
      val (copyMeths, applyMeths) =
        if (mods is Abstract) (Nil, Nil)
        else {
          val copyFirstParams = caseParams.map(vparam =>
            vparam.derivedValDef(Modifiers(TermParam), vparam.name, vparam.tpt, refOfDef(vparam)))
          val copyRestParamss = constr1.vparamss.tail.nestedMap(vparam =>
            vparam.derivedValDef(Modifiers(TermParam), vparam.name, vparam.tpt, EmptyTree))
          val applyParamss = constr1.vparamss.nestedMap(vparam =>
            vparam.derivedValDef(Modifiers(TermParam), vparam.name, vparam.tpt, vparam.rhs))
          val copyMeth =
            DefDef(synthetic, nme.copy, methTypeParams(cdef), copyFirstParams :: copyRestParamss, EmptyTree, creator(cdef))
          val applyMeth =
            DefDef(synthetic, nme.apply, methTypeParams(cdef), methParamss(cdef), EmptyTree, creator(cdef))
          (copyMeth :: Nil, applyMeth :: Nil)
      }
      val unapplyMeth = {
        val unapplyParam = makeSyntheticParameter(tpt = classTypeRef(cdef))
        DefDef(synthetic, nme.unapply, methTypeParams(cdef), (unapplyParam :: Nil) :: Nil, EmptyTree, This(EmptyTypeName))
      }
      updateCompanion(cname.toTermName, applyMeths ::: unapplyMeth :: Nil)
      addToClass(cdef, copyMeths ::: isDefinedMeth :: productArityMeth :: productElemMeths.toList)
    }

    def addToTemplate(templ: Template, stats: List[Tree]): Template =
      templ.derivedTemplate(templ.constr, templ.parents, templ.self, templ.body ++ stats)

    def addToClass(cdef: ClassDef, stats: List[Tree]): ClassDef =
      cdef.derivedClassDef(cdef.mods, cdef.name, cdef.tparams, addToTemplate(cdef.impl, stats))

    def addToModule(mdef: ModuleDef, stats: List[Tree]): ModuleDef =
      mdef.derivedModuleDef(mdef.mods, mdef.name, addToTemplate(mdef.impl, stats))

    def updateCompanion(name: TermName, newDefs: List[Tree]) =
      for (companion @ ModuleDef(_, `name`, _) <- enclosingStats) {
        expandedTrees(companion) = expandedTrees get companion match {
          case Some(Thicket(vdef :: (cdef: ClassDef) :: Nil)) =>
            Thicket(vdef, addToClass(cdef, newDefs))
          case none =>
            addToModule(companion, newDefs)
        }
      }

    def implicitWrapper(cdef: ClassDef) =
      DefDef(Modifiers(Synthetic | Implicit), cdef.name.toTermName,
          methTypeParams(cdef), methParamss(cdef), EmptyTree, creator(cdef))

    val tree1 = tree match {
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
      case mdef: ModuleDef =>
        desugarModuleDef {
          expandedTrees get mdef match {
            case Some(mdef1: ModuleDef) => mdef
            case _ => mdef
          }
        }
      case cdef: ClassDef =>
        val result = if (cdef.mods is Case) expandCaseClass(cdef) else cdef
        if (cdef.mods is Implicit) {
          if (ctx.owner is Package)
            ctx.error("implicit classes may not be toplevel", cdef.pos)
          Thicket(result :: implicitWrapper(cdef) :: Nil)
        }
        else result
      case _ =>
        tree
    }
    if (tree1 ne tree) expandedTrees(tree) = tree1
    tree1
  }

  def enterSyms(stats: List[Tree])(implicit ctx: Context): Unit = stats match {
    case (imp @ Import(expr, selectors)) :: rest =>
      val sym = createSymbol(imp, imp)
      enterSyms(rest)(ctx.fresh.withImport(ImportInfo(sym, selectors, ctx.scopeNestingLevel)))
    case stat :: rest =>
      for (expanded <- expansion(stat).toList) createSymbol(expanded, stat)
      enterSyms(rest)
    case Nil =>
  }



  class Completer(tree: Tree)(implicit ctx: Context) extends LazyType {
    def complete(sym: SymDenotation): Unit = {
      for (defn <- expandedTrees.getOrElse(tree, tree).toList if symOfTree(defn) == sym)
        defn match {
          case ValDef(mods, name, tpt, rhs) =>
            val (tpt1, rhs1) =
              if (tpt.isEmpty) {
                val rhs1 = typer.typedExpr(rhs)
                (TypeTree().withType(rhs1.tpe), TypedSplice(rhs1))
              }
              else (typer.typedType(tpt), rhs)
            defn.derivedValDef(mods, name, TypedSplice(tpt1), rhs1)


          }

      }
    }
  }
}