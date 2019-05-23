package dotty.tools
package dotc
package parsing

import dotty.tools.dotc._
import core._, ast._
import Trees._
import Contexts.Context

object parsePackage extends ParserTest {

  import ast.untpd._

  var nodes = 0

  val transformer = new UntypedTreeMap {
    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      nodes += 1
      tree match {
        case Ident(name) =>
          Ident(name)
        case This(name) =>
          This(name)
        case TypedSplice(t) =>
          TypedSplice(t)
        case SymbolLit(str) =>
          tree
        case InterpolatedString(id, segments) =>
          InterpolatedString(id, segments map transform)
        case mdef @ ModuleDef(name, impl) =>
          ModuleDef(name, transformSub(impl)).withMods(mdef.mods)
        case Function(params, body) =>
          Function(params map transform, body)
        case InfixOp(l, o, r) =>
          InfixOp(transform(l), o, transform(r))
        case PostfixOp(l, o) =>
          PostfixOp(transform(l), o)
        case PrefixOp(o, t) =>
          PrefixOp(o, transform(t))
        case Parens(t) =>
          Parens(transform(t))
        case Tuple(ts) =>
          Tuple(ts map transform)
        case WhileDo(cond, body) =>
          WhileDo(transform(cond), transform(body))
        case DoWhile(body, cond) =>
          DoWhile(transform(body), transform(cond))
        case ForYield(enums, expr) =>
          ForYield(enums map transform, transform(expr))
        case ForDo(enums, expr) =>
          ForDo(enums map transform, transform(expr))
        case GenFrom(pat, expr, filtering) =>
          GenFrom(transform(pat), transform(expr), filtering)
        case GenAlias(pat, expr) =>
          GenAlias(transform(pat), transform(expr))
        case PatDef(mods, pats, tpt, expr) =>
          PatDef(mods, pats map transform, transform(tpt), transform(expr))
        case ContextBounds(bounds, cxBounds) =>
          ContextBounds(transformSub(bounds), cxBounds map transform)
        case _ =>
          super.transform(tree)
      }
    }
  }

  def test() = {
    reset()
    nodes = 0
    val start = System.nanoTime()
    parseDir("./src")
    parseDir("./tests/scala2-library/src")
    val ms1 = (System.nanoTime() - start)/1000000
    val buf = parsedTrees map transformer.transform
    val ms2 = (System.nanoTime() - start)/1000000
    println(s"$parsed files parsed in ${ms1}ms, $nodes nodes transformed in ${ms2-ms1}ms, total trees created = ${Trees.ntrees}")
    ctx.reporter.printSummary(ctx)
  }

  def main(args: Array[String]): Unit = {
//    parse("/Users/odersky/workspace/scala/src/compiler/scala/tools/nsc/doc/model/ModelFactoryTypeSupport.scala")
    for (i <- 0 until 10) test()
  }
}
