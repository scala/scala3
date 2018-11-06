package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import scala.tasty.util.TreeTraverser
import dotty.tools.dotc.tastyreflect
import scala.collection.mutable.HashMap
import scala.meta.internal.{semanticdb => s}
import dotty.semanticdb.Scala.{Descriptor => d}
import dotty.semanticdb.Scala._

class SemanticdbConsumer extends TastyConsumer {
  var stack: List[String] = Nil

  /*
  val symbolsDefs : HashMap[String, Int] = HashMap()
  val symbolsVals : HashMap[String, Int] = HashMap()

  def insertPathDefDef(path: String): String = {
    if (symbolsDefs.contains(path)) {
      symbolsDefs += (path -> (symbolsDefs(path) + 1))
      "+" + (symbolsDefs(path) - 1)
    } else {
      symbolsDefs += (path -> 1)
      ""
    }
  }
  def insertPathValDef(path: String): String = {
    if (symbolsVals.contains(path)) {
      symbolsVals += (path -> (symbolsVals(path) + 1))
      "+" + (symbolsVals(path) - 1)
    } else {
      symbolsVals += (path -> 1)
      ""
    }
  }*/
  val semantic: s.TextDocument = s.TextDocument()
  var occurrences: Seq[s.SymbolOccurrence] = Seq()

  def toSemanticdb(text: String): s.TextDocument = {
    s.TextDocument(text = text, occurrences = occurrences)
  }


  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object Traverser extends TreeTraverser {
      val symbolsCache: HashMap[tasty.Symbol, String] = HashMap()
      val symbolPathsDisimbiguator: HashMap[String, Int] = HashMap()

      implicit class SymbolExtender(symbol: Symbol) {
        def isTypeParameter: Boolean = symbol match {
          case IsTypeSymbol(_) => symbol.flags.isParam
          case _               => false
        }

        def isType: Boolean = symbol match {
          case IsTypeSymbol(_) => true
          case _ => false
        }

        def isMethod: Boolean = symbol match {
          case IsDefSymbol(_) => true
          case _ => false
        }

        def isPackage: Boolean = symbol match {
          case IsPackageSymbol(_) => true
          case _ => false
        }

        def isTrait: Boolean = symbol.flags.isTrait

        def isValueParameter: Boolean = symbol.flags.isParam

        // TODO : implement it
        def isJavaClass: Boolean = false
      }

      val symbolsCache: HashMap[tasty.Symbol, String] = HashMap()
      val symbolPathsDisimbiguator: HashMap[String, Int] = HashMap()

      def disimbiguate(symbol_path: String): String = {
        if (symbolPathsDisimbiguator.contains(symbol_path)) {
          symbolPathsDisimbiguator +=
            (symbol_path -> (symbolPathsDisimbiguator(symbol_path) + 1))
          "(+" + (symbolPathsDisimbiguator(symbol_path) - 1) + ")"
        } else {
          symbolPathsDisimbiguator += (symbol_path -> 1)
          "()"
        }
      }
      def iterateParent(symbol: Symbol): String = {
        if (symbolsCache.contains(symbol)) {
          return symbolsCache(symbol)
        } else {
          val out_symbol_path =
            if (symbol.name == "<none>" || symbol.name == "<root>") then {
              // TODO had a "NoDenotation" test to avoid
              // relying on the name itself
              ""
            } else {
              val previous_symbol = iterateParent(symbol.owner)
              val next_atom =
              if (symbol.isPackage) {
                d.Package(symbol.name)
              } else if (symbol.isTypeParameter)  {
                d.TypeParameter(symbol.name)
              } else if (symbol.isMethod) {
                d.Method(symbol.name, disimbiguate(previous_symbol + symbol.name))
              } else if (symbol.isValueParameter) {
                d.Parameter(symbol.name)
              } else if (symbol.isType || symbol.isTrait) {
                d.Type(symbol.name)
              } else {
                d.Term(symbol.name)
              }

              Symbols.Global(previous_symbol, next_atom)
            }
          symbolsCache += (symbol -> out_symbol_path)
          out_symbol_path
        }
      }

      def addOccurence(symbol: Symbol,
                       type_symbol: s.SymbolOccurrence.Role,
                       range: s.Range): Unit = {
        //if (symbolsCache.contains(symbol)) return

        val symbol_path = iterateParent(symbol)
        if (symbol_path == "" || symbol.name == "<init>") return

        occurrences =
          occurrences :+
            s.SymbolOccurrence(
              Some(range),
              symbol_path,
              type_symbol
            )
      }

      def range(pos: Position, name: String): s.Range = {
        val range_end_column =
          if (name == "<init>") {
            pos.endColumn
          } else {
            pos.startColumn + name.length
          }

        s.Range(pos.startLine, pos.startColumn, pos.startLine, range_end_column)
      }

      def rangeExclude(range: Position, exclude: Position): s.Range = {
        def max(a: Int, b: Int): Int = { if (a > b) a else b }
        return s.Range(max(range.startLine, exclude.startLine),
                       max(range.startColumn, exclude.startColumn) + 1,
                       range.endLine,
                       range.endColumn)
      }

      def typetreeSymbol(typetree: TypeTree): Unit =
        typetree match {
          case TypeTree.Synthetic => ()
          case _ =>
            addOccurence(typetree.symbol,
                         s.SymbolOccurrence.Role.REFERENCE,
                         range(typetree.pos, typetree.symbol.name))
        }

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        //println(tree.pos.startColumn, tree.symbol.name, tree.pos.endColumn)
        tree match {
          case IsDefinition(body) => {
            if (tree.symbol.name != "<init>") {
              val _ = tree match {
                case DefDef(_, _, _, typetree, _) => typetreeSymbol(typetree)
                case ValDef(_, typetree, _)       => typetreeSymbol(typetree)
                case _                            => ()
              }
            }
            addOccurence(tree.symbol,
                         s.SymbolOccurrence.Role.DEFINITION,
                         range(tree.symbol.pos, tree.symbol.name))

            super.traverseTree(body)
          }

          case Term.Select(qualifier, _, _) => {
            val range = rangeExclude(tree.pos, qualifier.pos)
            addOccurence(tree.symbol, s.SymbolOccurrence.Role.REFERENCE, range)
            super.traverseTree(tree)
          }

          case Term.Ident(_) => {
            //println(tree.pos.startColumn, tree.pos.endColumn)
            //println(tree.namePos.startColumn, tree.namePos.endColumn)
            addOccurence(tree.symbol,
                         s.SymbolOccurrence.Role.REFERENCE,
                         range(tree.pos, tree.symbol.name))
            super.traverseTree(tree)
          }
          case PackageClause(_) =>
            addOccurence(tree.symbol,
                         s.SymbolOccurrence.Role.REFERENCE,
                         range(tree.pos, tree.symbol.name))
            super.traverseTree(tree)

          case tree =>
            super.traverseTree(tree)
        }
      }

    }

    Traverser.traverseTree(root)(reflect.rootContext)
  }

  def println(x: Any): Unit = Predef.println(x)

}
