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

    println("Full tree =========================")
    println(root.show)
    println("End of full tree ==================")

    def convertTypeTree(typeTree: reflect.TypeTree) : String = typeTree match {
      case reflect.TypeTree.Ident(id) => id
      //case reflect.TypeTree.Applied(tycon, args) => convertType(tycon) + "[" + args.map(x => convertType(x)).reduceLeft
      //case reflect.Type.AppliedType(tycon, args) =>
      case tpt => convertTypeOrBounds(tpt.tpe)
    }

    def convertTypeOrBounds(tpe: reflect.TypeOrBounds) : String = tpe match {
      case reflect.Type.AppliedType(tycon, args) => convertTypeOrBounds(tycon) + "[" + args.map(convertTypeOrBounds).foldLeft("")((x, y) => x + ", " + y) + "]"
      case reflect.Type.TypeRef(name, qualifier) => name //TODO: handle qualifier
      case tpe => tpe.toString
    }

    def traverse(level: Int, child: reflect.Tree) : String = {

      def findDefDef(level: Int, child: reflect.Tree) : String = {
        child match {
          case IsDefinition(ddef @ DefDef(name, tparams, vparamss, tpt, rhs)) =>
            (0 until level).map(_ => "  ").foldLeft("")(_+_) +
            name +
            "(" +
            (vparamss match {
              case Nil | List(Nil) => "" //Difference?
              case List(args) => args.map{case ValDef(vname, vtype, _) => vname + ": " + convertTypeTree(vtype)}.reduce((x, y) => x + ", " + y)
              case _ => "FAILED MATCH FOR VPARAMS DEFDEF"
            }) +
            ") : " +
            convertTypeTree(tpt) + "\n"
          case _ => "FAILED MATCH FOR DEFDEF"
        }
      }
      def findValDef(level: Int, child: reflect.Tree) : String = {
        child match {
          case IsDefinition(vdef @ ValDef(name, tpt, rhs)) =>
            (0 until level).map(_ => "  ").foldLeft("")(_+_) +
            name + " : " +
            convertTypeTree(tpt)
          case _ => "FAILED MATCH FOR VALDEF"
        }
      }

      (0 until level).map(_ => "  ").foldLeft("")(_+_) +
      (child match {
        case reflect.Term.Ident(name) => name
        case reflect.PackageClause(pid, stats) =>
          "Package: "+ traverse(level, pid) + "\n" +
          stats.map(traverse(level+1, _)).foldLeft("")(_+_)
        case reflect.Import(impliedOnly, expr, selectors) =>
          "import " + expr + selectors + "\n"
        case reflect.ClassDef(name, constr, parents, derived, self, body) =>
          "Class: " + name + "\n" +
          (0 until level+1).map(_ => "  ").foldLeft("")(_+_) +
          "Methods:" + "\n" +
          body.map(findDefDef(level+2, _)).foldLeft("")(_+_) +
          (0 until level+1).map(_ => "  ").foldLeft("")(_+_) +
          "Values:" + "\n" +
          body.map(findValDef(level+2, _)).foldLeft("")(_+_)
        case _ =>
          "No match in traverse" + "\n"
      })
    }

    print(traverse(0, root))
    println
  }
}