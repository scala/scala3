import scala.quoted.*

inline def ownerWorks(in: Int): Any =
    ${ownerWorksImpl('in)}

transparent inline def ownerDoesNotWork(in: Int): Any =
    ${ownerWorksImpl('in)}

def ownerWorksImpl(in: Expr[Int])(using Quotes): Expr[String] =
  import quotes.reflect.*
  val position = Position.ofMacroExpansion
  val file = position.sourceFile
  val owner0 = Symbol.spliceOwner.maybeOwner
  val ownerName = owner0.tree match {
    case ValDef(name, _, _) =>
      name
    case DefDef(name, _, _, _) =>
      name
    case t => report.errorAndAbort(s"unexpected tree shape: ${t.show}")
  }
  val path = file.path
  val line = position.startLine
  val column = position.startColumn
  val v = in.valueOrAbort
  val out = Expr(s"val $ownerName $v: $file @ ${position.startLine}")
  out


