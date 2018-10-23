package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import scala.tasty.util.TreeTraverser
import dotty.tools.dotc.tastyreflect
import scala.collection.mutable.HashMap
import scala.meta.internal.{semanticdb => s}

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

      def packageDefToOccurence(term: Term): String = {
        //println(term, term.pos.start, term.pos.end)
        val Term.Ident(id) = term
        return stack.head + id + "/"
      }

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
            if (symbol.name == "<none>") then {
              // TODO had a "NoDenotation" test to avoid
              // relying on the name itself
              ""
            } else if (symbol.name == "<root>") then {
              // TODO had a "NoDenotation" test to avoid
              // relying on the name itself
              ""
            } else {
              val previous_symbol = iterateParent(symbol.owner)
              val next_atom =
                symbol match {
                  case IsPackageSymbol(symbol) => symbol.name + "/"
                  case IsClassSymbol(symbol)   => symbol.name + "#"
                  case IsDefSymbol(symbol) =>
                    symbol.name + disimbiguate(previous_symbol + symbol.name) + "."
                  case IsValSymbol(symbol) => symbol.name + "."
                  case owner => {
                    ""
                  }
                }
              previous_symbol + next_atom
            }
          symbolsCache += (symbol -> out_symbol_path)
          out_symbol_path
        }
      }

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        val previous_path = stack.head

        tree match {
          case IsDefinition(body) =>
            //println("[definition] ", body)
            val symbol_path = iterateParent(tree.symbol)

            val range =
              if (tree.symbol.name == "<init>") {
                s.Range(tree.symbol.pos.startLine,
                        tree.symbol.pos.startColumn,
                        tree.symbol.pos.startLine,
                        tree.symbol.pos.endColumn)
              } else {
                s.Range(tree.symbol.pos.startLine,
                        tree.symbol.pos.startColumn,
                        tree.symbol.pos.startLine,
                        tree.symbol.pos.startColumn + tree.symbol.name.length)
              }
            occurrences =
              occurrences :+
                s.SymbolOccurrence(
                  Some(range),
                  symbol_path,
                  s.SymbolOccurrence.Role.DEFINITION
                )
            super.traverseTree(body)
          case tree =>
            super.traverseTree(tree)
        }
      }

    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }

  def println(x: Any): Unit = Predef.println(x)

}
