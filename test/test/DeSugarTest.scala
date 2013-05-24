package test

import scala.reflect.io._
import dotty.tools.dotc.util._
import dotty.tools.dotc.core._
import dotty.tools.dotc.parsing._
import Tokens._, Parsers._
import org.junit.Test
import dotty.tools.dotc._
import ast.Trees._

import scala.collection.mutable.ListBuffer

class DeSugarTest extends ParserTest {

  import dotty.tools.dotc.ast.untpd._

  import Mode._

  object DeSugar extends TreeTransformer {
    var curMode: Mode.Value = Mode.Expr
    def withMode[T](mode: Mode.Value)(op: => T) = {
      val saved = curMode
      curMode = mode
      try op
      finally curMode = saved
    }

    def transform(tree: Tree, mode: Mode.Value): Tree = withMode(mode) { transform(tree) }
    def transform(trees: List[Tree], mode: Mode.Value): List[Tree] = withMode(mode) { transform(trees) }

    override def transform(tree: Tree): Tree = {
      val tree1 = desugar(tree, curMode)
      if (tree1 ne tree) {
        //println(s"repeat desugar ${tree1.getClass}")
        transform(tree1)
      }
      else tree1 match {
        case TypedSplice(t) =>
          tree1
        case PostfixOp(od, op) =>
          PostfixOp(transform(od), op)
        case Select(qual, name) =>
          tree1.derivedSelect(transform(qual, Expr), name)
        case Apply(fn, args) =>
          tree1.derivedApply(transform(fn, Expr), transform(args))
        case TypeApply(fn, args) =>
          tree1.derivedTypeApply(transform(fn, Expr), transform(args, Type))
        case New(tpt) =>
          tree1.derivedNew(transform(tpt, Type))
        case Typed(expr, tpt) =>
          tree1.derivedTyped(transform(expr), transform(tpt, Type))
        case CaseDef(pat, guard, body) =>
          tree1.derivedCaseDef(transform(pat, Pattern), transform(guard), transform(body))
        case SeqLiteral(elempt, elems) =>
          tree1.derivedSeqLiteral(transform(elempt, Type), transform(elems))
        case UnApply(fun, args) =>
          tree1.derivedUnApply(transform(fun, Expr), transform(args))
        case ValDef(mods, name, tpt, rhs) =>
          tree1.derivedValDef(mods, name, transform(tpt, Type), transform(rhs))
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          tree1.derivedDefDef(mods, name, transformSub(tparams), vparamss mapConserve (transformSub(_)), transform(tpt, Type), transform(rhs))
        case TypeDef(mods, name, tparams, rhs) =>
          tree1.derivedTypeDef(mods, name, transformSub(tparams), transform(rhs, Type))
        case Template(constr, parents, self, body) =>
          tree1.derivedTemplate(transformSub(constr), transform(parents), transformSub(self), transform(body, Expr))
        case ClassDef(mods, name, tparams, impl) =>
          tree1.derivedClassDef(mods, name, transformSub(tparams), transformSub(impl))
        case tree1 =>
          super.transform(tree1)
      }
    }
  }

  def firstClass(stats: List[Tree]): String = stats match {
    case Nil => "<empty>"
    case ClassDef(_, name, _, _) :: _ => name.toString
    case ModuleDef(_, name, _) :: _ => name.toString
    case (pdef: PackageDef) :: _ => firstClass(pdef)
    case stat :: stats => firstClass(stats)
  }

  def firstClass(tree: Tree): String = tree match {
    case PackageDef(pid, stats) =>
      pid.show + "." + firstClass(stats)
    case _ => "??? "+tree.getClass
  }

  def desugarTree(tree: Tree): Tree = {
    //println("***** desugaring "+firstClass(tree))
    DeSugar.transform(tree)
  }

  def desugarAll() = parsedTrees foreach (desugarTree(_).show)
}