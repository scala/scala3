package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set

class TastydocConsumer extends TastyConsumer {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    println("Full tree =========================")
    println(root.show)
    println("End of full tree ==================")

    def traverse(child: reflect.Tree) : Container = {

      def convertToContainer(reflectType : reflect.Statement) : Container = reflectType match {
        case reflect.DefDef(name, typeParams, paramss, tpt, rhs) =>
          new DefContainer(name +
            "(" +
            (paramss match {
              case Nil | List(Nil) => "" //Difference?
              case List(args) => args.map{case ValDef(vname, vtype, _) => vname + ": " + convertTypeTree(vtype)}.reduce((x, y) => x + ", " + y)
              case _ => "FAILED MATCH FOR VPARAMS DEFDEF"
            }) +
            ") : " +
            convertTypeTree(tpt))

        case reflect.ValDef(name, tpt, rhs) =>
          new ValContainer(
            name +
            " : " +
            convertTypeTree(tpt)
          )
      }

      def convertTypeTree(typeTree: reflect.TypeTree) : String = typeTree match {
        case reflect.TypeTree.Ident(id) => id
        //case reflect.TypeTree.Applied(tycon, args) => convertType(tycon) + "[" + args.map(x => convertType(x)).reduceLeft
        //case reflect.Type.AppliedType(tycon, args) =>
        case tpt => convertTypeOrBounds(tpt.tpe)
      }

      def convertTypeOrBounds(tpe: reflect.TypeOrBounds) : String = tpe match {
        case reflect.Type.AppliedType(tycon, args) => convertTypeOrBounds(tycon) + "[" + (args.map(convertTypeOrBounds)).reduce((x, y) => x + ", " + y) + "]"
        case reflect.Type.TypeRef(name, qualifier) => name //TODO: handle qualifier
        case tpe => tpe.toString
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
            case x :: xs => iterBody(xs, defdef, valdef, typedef)
          }
          val (defdef, valdef, typedef) = iterBody(body, Nil, Nil, Nil)
          new ClassContainer(sign, defdef, valdef, typedef)
        case _ => new MissingMatchContainer()
      }
    }

    print(format(traverse(root), 0))
  }
}