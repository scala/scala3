import dotty.tools.dotc._
import core._
import Types._
import Contexts._
import Symbols._
import Decorators._

object Test {
  def f(implicit ctx: Context) = {
    val badType =
      RefinedType(
        ctx.requiredClassRef("scala.collection.AbstractIterator"),
        "GroupedIterator".toTypeName,
        TypeRef(ctx.requiredClassRef("scala.collection.AbstractIterator"), "GroupedIterator".toTypeName))
    badType.member("GroupedIterator".toTypeName)
   // badType.member("T".toTypeName)
  }
  def main(args: Array[String]): Unit = {
    implicit val ctx = (new ContextBase).initialCtx
    ctx.base.initialize()
    try f
    catch {
      case ex: AssertionError => println("malformed type")
    }
  }
}
