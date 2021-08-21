package dotty.tools.scaladoc
package snippets

import scala.quoted._
import dotty.tools.scaladoc.tasty.SymOps._
import dotty.tools.dotc.core._
import dotty.tools.dotc.util.{ SourceFile => CSourceFile, NoSource }

class SnippetCompilerDataCollector[Q <: Quotes](val qctx: Q):
  import qctx.reflect._
  given qctx.type = qctx

  def getSourceFile(sym: Symbol): CSourceFile =
    given ctx: Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    sym match
      case csym: Symbols.Symbol => csym.source(using ctx)
      case _ =>
        report.warning(s"Can't cast symbol $sym to compiler symbol. This is a bug of snippet compiler, please create an issue on dotty repository.")
        NoSource

  def getSnippetCompilerData(sym: Symbol, originalSym: Symbol): SnippetCompilerData =
    val packageName = sym.packageName
    if !sym.isPackageDef then sym.tree match {
      case c: qctx.reflect.ClassDef =>
        import dotty.tools.dotc
        import dotty.tools.dotc.core.Decorators._
        given dotc.core.Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
        val printer: dotc.printing.Printer = SelfTypePrinter()
        val classSym = c.symbol.asInstanceOf[Symbols.ClassSymbol]

        def getOwners(sym: Symbols.ClassSymbol): Seq[Symbols.ClassSymbol] = Seq(sym) ++
          (if sym.owner.isClass && !sym.owner.is(dotc.core.Flags.Package) then getOwners(sym.owner.asInstanceOf[Symbols.ClassSymbol]) else Nil)

        def collectNames(tp: Types.Type): Seq[String] = tp match {
          case Types.AndType(t1, t2) => collectNames(t1) ++ collectNames(t2)
          case Types.AppliedType(tpe, _) => collectNames(tpe)
          case Types.AnnotatedType(tpe, _) => collectNames(tpe)
          case t: Types.NamedType => if t.symbol.is(dotc.core.Flags.Module) then Seq() else Seq(t.symbol.name.show)
          case t: Types.ThisType => Seq(t.cls.name.show)
          case _ => Seq()
        }

        val allSyms = getOwners(classSym)

        val allProcessed = allSyms.map { cSym =>
          def createTypeConstructor(tpe: Types.Type, topLevel: Boolean = true): String = tpe match {
              case t @ Types.TypeBounds(upper, lower) => lower match {
                case l: Types.HKTypeLambda =>
                  (if topLevel then "" else "?") + l.paramInfos.map(p => createTypeConstructor(p, false)).mkString("[",", ","]")
                case _ => (if topLevel then "" else "_")
              }
            }
          val classType =
            val ct = cSym.classInfo.selfType.toText(printer).show.replace(".this","").replace("\n", " ").stripPrefix(s"$packageName.")
            Some(ct)
          val classNames = collectNames(cSym.classInfo.selfType)
          val classGenerics = Option.when(
              !cSym.typeParams.isEmpty
            )(
              cSym.typeParams.map(_.typeRef).map(t =>
                t.show +
                createTypeConstructor(t.asInstanceOf[Types.TypeRef].underlying)
              ).mkString("[",", ","]")
            )
          SnippetCompilerData.ClassInfo(classType, classNames, classGenerics)
        }
        val firstProcessed = allProcessed.head
        SnippetCompilerData(packageName, allProcessed.reverse, Nil, position(hackGetPositionOfDocstring(using qctx)(originalSym)))
      case _ => getSnippetCompilerData(sym.maybeOwner, originalSym)
    } else SnippetCompilerData(packageName, Nil, Nil, position(hackGetPositionOfDocstring(using qctx)(originalSym)))

  private def position(p: Option[qctx.reflect.Position]): SnippetCompilerData.Position =
    p.fold(SnippetCompilerData.Position(0, 0))(p => SnippetCompilerData.Position(p.startLine, p.startColumn))

  private def hackGetPositionOfDocstring(using Quotes)(s: qctx.reflect.Symbol): Option[qctx.reflect.Position] =
    import dotty.tools.dotc.core.Comments.CommentsContext
    import dotty.tools.dotc
    given ctx: Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val docCtx = ctx.docCtx.getOrElse {
      throw new RuntimeException(
        "DocCtx could not be found and documentations are unavailable. This is a compiler-internal error."
      )
    }
    s.pos.flatMap { pos =>
      docCtx.docstring(s.asInstanceOf[Symbols.Symbol]).map { docstring =>
        dotty.tools.dotc.util.SourcePosition(
          pos.sourceFile.asInstanceOf[dotty.tools.dotc.util.SourceFile],
          docstring.span
        ).asInstanceOf[qctx.reflect.Position]
      }
    }