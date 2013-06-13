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
import annotation.tailrec
import language.implicitConversions
import desugar.Mode

trait TyperContextOps { ctx: Context => }


class Typer extends Namer {

  import tpd._

  def typedModifiers(mods: untpd.Modifiers): Modifiers = ???

  def typedValDef(vdef: untpd.ValDef, sym: Symbol)(implicit ctx: Context) = {
    val Trees.ValDef(mods, name, tpt, rhs) = vdef
    val mods1 = typedModifiers(mods)
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    val pt = if (sym.exists) sym.symRef else NoType
    vdef.withType(pt).derivedValDef(mods1, name, tpt1, rhs1)
  }

  def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
    val Trees.DefDef(mods, name, tparams, vparamss, tpt, rhs) = ddef
    val mods1 = typedModifiers(mods)
    val tparams1 = tparams mapconserve (typed(_).asInstanceOf[TypeDef])
    val vparamss1 = vparamss.mapconserve(_ mapconserve (typed(_).asInstanceOf[ValDef]))
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    ddef.withType(sym.symRef).derivedDefDef(mods1, name, tparams1, vparamss1, tpt1, rhs1)
  }

  def typedTypeDef(tdef: untpd.TypeDef, sym: Symbol)(implicit ctx: Context): TypeDef = {
    val Trees.TypeDef(mods, name, rhs) = tdef
    val mods1 = typedModifiers(mods)
    val rhs1 = typedType(rhs)
    tdef.withType(sym.symRef).derivedTypeDef(mods1, name, rhs1)
  }

  def typedClassDef(cdef: untpd.ClassDef, cls: ClassSymbol)(implicit ctx: Context) = {
    val Trees.ClassDef(mods, name, impl @ Template(constr, parents, self, body)) = cdef
    val mods1 = typedModifiers(mods)
    val constr1 = typed(constr).asInstanceOf[DefDef]
    val parents1 = parents mapconserve (typed(_))
    val self1 = self.withType(NoType).derivedValDef(
      typedModifiers(self.mods), self.name, typed(self.tpt), EmptyTree)

    val localDummy = ctx.newLocalDummy(cls, impl.pos)
    val body1 = typedStats(body, localDummy)(inClassContext(cls, self.name))
    val impl1 = impl.withType(localDummy.symRef).derivedTemplate(
      constr1, parents1, self1, body1)

    cdef.withType(cls.symRef).derivedClassDef(mods1, name, impl1)

    // todo later: check that
    //  1. If class is non-abstract, it is instantiatable:
    //  - self type is s supertype of own type
    //  - all type members have consistent bounds
    // 2. all private type members have consistent bounds
    // 3. Types do not override classes.
    // 4. Polymorphic type defs override nothing.
  }

  def typedImport(imp: untpd.Import, sym: Symbol)(implicit ctx: Context): Import = {
    val expr1 = typed(imp.expr)
    imp.withType(sym.symRef).derivedImport(expr1, imp.selectors)
  }

  def typedExpanded(tree: untpd.Tree, mode: Mode.Value = Mode.Expr, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
    val sym = symOfTree.remove(tree).getOrElse(NoSymbol)
    sym.ensureCompleted()
    def localContext = ctx.fresh.withOwner(sym)
    typedTree remove tree match {
      case Some(tree1) => tree1
      case none => tree match {
        case tree: untpd.ValDef =>
          typedValDef(tree, sym)(localContext)
        case tree: untpd.DefDef =>
          val typer1 = nestedTyper.remove(sym).get
          typer1.typedDefDef(tree, sym)(localContext.withTyper(typer1))
        case tree: untpd.TypeDef =>
          typedTypeDef(tree, sym)(localContext.withNewScope)
        case tree: untpd.ClassDef =>
          typedClassDef(tree, sym.asClass)(localContext)
        case tree: untpd.Import =>
          typedImport(tree, sym)
        case tree: untpd.TypeTree =>
          if (!tree.isEmpty) typed(tree.original, Mode.Type, pt)
          else {
            assert(pt != WildcardType)
            tree.withType(pt)
          }
        case untpd.EmptyTree =>
          tpd.EmptyTree
      }
    }
  }

  def typed(tree: untpd.Tree, mode: Mode.Value = Mode.Expr, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
    val xtree =
      tree match {
        case tree: untpd.MemberDef =>
          expandedTree remove tree match {
            case Some(xtree) => xtree
            case none => tree
          }
        case _ => tree
    }
    typedExpanded(xtree, mode, pt)
  }

  def typedStats(stats: List[untpd.Tree], exprOwner: Symbol)(implicit ctx: Context): List[tpd.Tree] = {
    val buf = new mutable.ListBuffer[Tree]
    @tailrec def traverse(stats: List[untpd.Tree])(implicit ctx: Context): List[Tree] = stats match {
      case (imp: untpd.Import) :: rest =>
        val imp1 = typed(imp)
        buf += imp1
        traverse(rest)(importContext(imp1.symbol, imp.selectors))
      case (mdef: untpd.MemberDef) :: rest =>
        buf += typed(mdef)
        traverse(rest)
      case stat :: rest =>
        buf += typed(stat)(ctx.fresh.withOwner(exprOwner))
        traverse(rest)
      case _ =>
        buf.toList
    }
    traverse(stats)
  }

  def typedExpr(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, Mode.Expr, pt)
  def typedType(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, Mode.Type, pt)

}