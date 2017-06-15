package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test
import dotc.ast.tpd
import dotc.core.Contexts.Context

class InjectableTreeTests extends ReplTest {

  def extractMembers(obj: tpd.TypeDef)(implicit ctx: Context): List[tpd.MemberDef] =
    obj.rhs match {
      case tmpl: tpd.Template =>
        tmpl.body.collect { case tree: tpd.MemberDef => tree }
      case _ => Nil
    }


  @Test def crashCheck = {
    implicit val ctx = myCtx
    val injTree = InjectableTree()
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")
    val symName = injTree.obj.trees(1).symbol.show
    assert(symName == "object ReplSession", symName)
  }

  @Test def injectOnce = {
    implicit val ctx = myCtx

    val injTree = InjectableTree()
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")
    val typer = new ReplTyper(myCtx)

    val tt @ TypedTrees(_,_, newCtx) = typer.typeCheck(parsed, 0)
    val InjectableTree(tpd.Thicket(List(_, obj: tpd.TypeDef)), 0) =
      InjectableTree.patch(injTree, tt)(newCtx)

    val members = extractMembers(obj)

    assert(members.length == 1, s"Wrong amount of members in object: $members")

    members.head match {
      case tpd.DefDef(name, _, _, _, _) => assert(name.show == "foo")
      case _ => assert(false, "incorrect shit yo")
    }
  }
}
