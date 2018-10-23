package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import scala.tasty.util.TreeTraverser
import dotty.tools.dotc.tastyreflect
import scala.collection.mutable.HashMap

class SemanticdbConsumer extends TastyConsumer {
  var stack : List[String] = Nil
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
  }

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object Traverser extends TreeTraverser {

      def packageDefToOccurence(term: Term): String = {
        //println(term, term.pos.start, term.pos.end)
        val Term.Ident(id) = term
        return stack.head + id + "/"
      }

      def iterateParent(symbol: Symbol): String = {
        if (symbol.name == "<none>") then {
          // TODO had a "NoDenotation" test to avoid
          // relying on the name itself
          ""
        } else {
          val previous_symbol = iterateParent(symbol.owner)
          val next_atom =
          symbol match {
          case IsPackageSymbol(symbol) => symbol.name + "/"
          case IsClassSymbol(symbol) => symbol.name + "#"
          case IsDefSymbol(symbol) => symbol.name + "."
          case IsValSymbol(symbol) => symbol.name + "."
          case owner => {
            ""
          }
          }
          previous_symbol + next_atom
        }
      }

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
      val previous_path = stack.head

        tree match {
          /*case IsClassDef(body) =>
            val ClassDef(name, _, _, _, _) = body
            //println("[classdef] ", body)
            val path = stack.head + name + "#"
            println(path)
            stack = path :: stack
            super.traverseTree(body)
            stack = stack.tail
          case IsTypeDef(body) =>
            println("[typedef] ", body)
            super.traverseTree(body)
          case IsDefDef(body) =>
            val DefDef(name, _, _, _, _) = body
            val def_atom =
            name match {
              case "<init>" => "`<init>`"
              case _ => name
            }
            val path_repr = stack.head + def_atom
            val path = path_repr + "(" + insertPathDefDef(path_repr) + ")."
            println(path)
            //println("[defdef] ", body)
            stack = path :: stack
            super.traverseTree(body)
            stack = stack.tail
          case IsValDef(body) =>
            val ValDef(name, _, _) = body
            val path_repr = stack.head + name
            val path = path_repr + "(" + insertPathValDef(path_repr) + ")"
            println(path)
            //println("[defdef] ", body)
            stack = path :: stack
            super.traverseTree(body)
            stack = stack.tail
          case IsPackageDef(body) =>
            println("[packagedef] ", body)
            super.traverseTree(body)*/
          case IsDefinition(body) =>
            //println("[definition] ", body)
            println(iterateParent(tree.symbol))
            super.traverseTree(body)
          /*case IsPackageClause(body) =>
            //println(body.pos.start, body.pos.end)
            val PackageClause(name, list_tree : List[Tree]) = body
            //println(tree)
            val path = packageDefToOccurence(name)
            println(path)
            stack = path :: stack
            // call to traverse tree instead of super.traverseTree to avoid
            // skipping this child entirely (super.xx will traverse subtrees)
            //list_tree.foreach{traverseTree}
          /*case IsTerm(body) =>
            //println("[term] ", body)
            super.traverseTree(body)*/
            // iterating this way will probably make us see terms we don't want
            super.traverseTree(body)
            stack = stack.tail*/
          case tree =>
            super.traverseTree(tree)
        }
      }

    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }

  def println(x: Any): Unit = Predef.println(x)

}
