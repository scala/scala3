package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set

class TastydocConsumer extends TastyConsumer {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

  //   root match {
  //     case IsClassDef(_) => println("I am reflecting on a class!")
  //     case _ => println("I am reflecting not on a class?"); apply/
  //   }

  //   println("Seems I am reflecting on:")
  //   println(root.show)

  //   object Traverser extends TreeTraverser {

  //     override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
  //       case IsDefinition(tree) =>
  //         println(tree.show)
  //         super.traverseTree(tree)
  //       case tree =>
  //         super.traverseTree(tree)
  //     }

  //   }
  //   Traverser.traverseTree(root)(reflect.rootContext)
  // }

    // println("Full tree =========================")
    // println(root.show)
    // println("End of full tree ==================")

    def printType(typeTree: reflect.TypeTree) : Unit = typeTree match {
      //case Ident(id) => print(id)
      case _ => print(typeTree)
    }

    def traverse(level: Int, child: reflect.Tree) : Unit = {

      def findDefDef(level: Int, child: reflect.Tree) : Unit = {
        child match {
          case IsDefinition(ddef @ DefDef(name, tparams, vparamss, tpt, rhs)) =>
            (0 until level).foreach(_ => print("  "))
            print(name)
            print("(")
            vparamss match {
              case Nil | List(Nil) => //Difference?
              case List(args) => print(args.map{case ValDef(vname, vtype, _) => vname + ": " + vtype}.reduce((x, y) => x + ", " + y))
              case _ => println("FAILED MATCH FOR VPARAMS DEFDEF")
            }
            print(")")
            print(" : ")
            printType(tpt)
            println
          case _ =>
        }
      }
      def findValDef(level: Int, child: reflect.Tree) : Unit = {
        child match {
          case IsDefinition(vdef @ ValDef(name, tpt, rhs)) =>
            (0 until level).foreach(_ => print("  "))
            print(name + " : ")
            printType(tpt)
          case _ =>
        }
      }

      (0 until level).foreach(_ => print("  "))
      child match {
        case IsPackageClause(clause @ PackageClause(pid, stats)) =>
          println("Package: "+ pid)
          stats.foreach(traverse(level+1, _))
        case Import(expr, selectors) =>
          println("import " + selectors + expr)
        case IsDefinition(cdef @ ClassDef(name, constr, parents, derived, self, body)) =>
          println("Class: " + name)
          (0 until level+1).foreach(_ => print("  "))
          println("Methods:")
          body.foreach(findDefDef(level+2, _))
          (0 until level+1).foreach(_ => print("  "))
          println("Values:")
          body.foreach(findValDef(level+2, _))
        case _ =>
          println("No match in traverse")
      }
    }

    traverse(0, root)
    println
  }
}