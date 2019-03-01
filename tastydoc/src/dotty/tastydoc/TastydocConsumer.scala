package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.annotation.tailrec

import java.io._

class TastydocConsumer extends TastyConsumer {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    println("Full tree =========================")
    println(root.show)
    println("End of full tree ==================")

    def traverse(child: reflect.Tree) : Container = {

      def convertToContainer(reflectType : reflect.Statement) : Container = reflectType match {
        case reflect.DefDef(name, typeParams, paramss, tpt, rhs) =>

          @tailrec def handleParams(ls: List[List[ValDef]], str: String) : String = ls match {
            case Nil => str
            case List()::xs => handleParams(xs, str + "()")
            case args::xs => handleParams(xs, str + "(" + args.map{case ValDef(vname, vtype, _) => vname + ": " + convertTypeTree(vtype)}.reduce((x, y) => x + ", " + y) + ")")
          }
          new DefContainer(name +
            handleParams(paramss, "") +
            " : " +
            convertTypeTree(tpt)
          )

        case reflect.ValDef(name, tpt, rhs) =>
          new ValContainer(
            name +
            " : " +
            convertTypeTree(tpt)
          )
      }

      def convertTypeTree(typeTree: reflect.TypeTree) : String = typeTree match {
        case _ => typeTree.tpe.showCode.replaceAll("\u001B\\[[;\\d]*m", "") //TODO: Other way to remove color?
        //case reflect.TypeTree.Ident(id) => id
        //case reflect.TypeTree.Applied(tycon, args) => convertType(tycon) + "[" + args.map(x => convertType(x)).reduceLeft
        //case reflect.Type.AppliedType(tycon, args) =>
        //case tpt => convertTypeOrBounds(tpt.tpe)
      }

      def convertTypeOrBounds(tpe: reflect.TypeOrBounds) : String = tpe match {
        case reflect.Type.AppliedType(tycon, args) => convertTypeOrBounds(tycon) + "[" + (args.map(convertTypeOrBounds)).reduce((x, y) => x + ", " + y) + "]"
        case reflect.Type.TypeRef(name, qualifier) => name //TODO: handle qualifier
        case tpe => tpe.show
      }

      child match {
        //case reflect.Term.Ident(name) => name
        case reflect.PackageClause(pid, stats) =>
          //"Package: "+ traverse(level, pid) + "\n" +
          //stats.map(traverse(level+1, _)).foldLeft("")(_+_)
          new PackageContainer("package " + pid, stats.map(traverse(_))) //TODO: Optional new?
        case reflect.Import(impliedOnly, expr, selectors) =>
          new ImportContainer("import " + expr + selectors)
        case reflect.ClassDef(name, constr, parents, derived, self, body) =>
          //TODO: Generic type
          //TODO: TypeDef
          //TODO: Classes inside class
          val sign = "class: " + name
          def iterBody(body: List[reflect.Statement], defdef: List[Container], valdef: List[Container], typedef: List[Container]) : (List[Container], List[Container], List[Container]) = body match {
            case Nil => (defdef.reverse, valdef.reverse, typedef.reverse) //TODO: More efficient than reverse?
            case (x @ reflect.DefDef(_, _, _, _, _)) :: xs => iterBody(xs, convertToContainer(x)::defdef, valdef, typedef)
            case (x @ reflect.ValDef(_, _, _)) :: xs => iterBody(xs, defdef, convertToContainer(x)::valdef, typedef)
            //case (x @ reflect.TypeDef())
            case x :: xs => iterBody(xs, defdef, valdef, typedef)
          }
          val (defdef, valdef, typedef) = iterBody(body, Nil, Nil, Nil)
          new ClassContainer(sign, defdef, valdef, typedef)
        case _ => new MissingMatchContainer()
      }
    }

    print(formatToMarkdown(traverse(root), 0))
    val pw = new PrintWriter(new File("./tastydoc/docoutputtest.md" ))
    pw.write(formatToMarkdown(traverse(root), 0))
    pw.close()
  }
}