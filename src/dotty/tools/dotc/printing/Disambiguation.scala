package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Contexts._
import collection.mutable
import scala.annotation.switch

object Disambiguation {

  private class State {
    var hasConflicts = false
    val symString = new mutable.HashMap[Symbol, String]
    val variants = new mutable.HashMap[String, mutable.ListBuffer[Symbol]]
  }

  def newPrinter: Context => Printer = {
    val state = new State
    new Printer(state)(_)
  }

  class Printer(state: State)(_ctx: Context) extends RefinedPrinter(_ctx) {
    import state._

    override def simpleNameString(sym: Symbol): String = {
      if ((sym is ModuleClass) && sym.sourceModule.exists) simpleNameString(sym.sourceModule)
      else symString.getOrElse(sym, recordedNameString(sym))
    }

    private def rawNameString(sym: Symbol) = super.simpleNameString(sym)

    private def recordedNameString(sym: Symbol): String = {
      val str = rawNameString(sym)
      val existing = variants.getOrElse(str, new mutable.ListBuffer[Symbol])
        // Dotty deviation: without a type parameter on ListBuffer, inference
        // will compute ListBuffer[Symbol] | ListBuffer[Nothing] as the type of "existing"
        // and then the assignment to variants below will fail.
        // We need to find a way to avoid such useless inferred types.
      if (!(existing contains sym)) {
        hasConflicts |= existing.nonEmpty
        variants(str) = (existing += sym)
      }
      str
    }

    def disambiguated(): Boolean = {
      val res = hasConflicts
      while (hasConflicts) disambiguate()
      res
    }

    private def qualifiers: Stream[String] =
      Stream("", "(some other)", "(some 3rd)") ++ (Stream.from(4) map (n => s"(some ${n}th)"))

    private def disambiguate(): Unit = {
      def update(sym: Symbol, str: String) = if (!(symString contains sym)) symString(sym) = str
      def disambiguated(sym: Symbol, owner: Symbol) = s"${rawNameString(sym)}(in ${simpleNameString(owner)})"
      hasConflicts = false
      for ((name, vs) <- variants.toList)
        if (vs.tail.nonEmpty) {
          for ((owner, syms) <- vs.groupBy(_.effectiveOwner)) {
            if (syms.tail.isEmpty) update(syms.head, disambiguated(syms.head, owner))
            else
              for {
                (kind, syms1) <- syms.groupBy(kindString)
                (sym, qual) <- syms1 zip qualifiers
              } {
                update(sym, s"$qual$kind ${disambiguated(sym, owner)}")
              }
          }
        }
    }
  }

  def disambiguated(op: Context => String)(implicit ctx: Context): String = {
    val dctx = ctx.printer match {
      case dp: Printer => ctx
      case _ => ctx.fresh.setPrinterFn(newPrinter)
    }
    val res = op(dctx)
    dctx.printer match {
      case dp: Printer if dp.disambiguated() => op(dctx)
      case _ => res
    }
  }
}
