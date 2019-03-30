package dotty.tastydoc

import scala.tasty.Reflection

object representations {

  trait Representation {
    val name : String
    val path : List[String]
    val comments: String
  }

  trait Parents {
    val parents : List[String]
  }

  trait Members {
    val members : List[Representation]
  }

  trait Modifiers {
    val modifiers: List[String]

    val isPrivate: Boolean = modifiers.contains("private")

    def isProtected: Boolean = modifiers.contains("protected")
  }

  trait Companion {
    val hasCompanion: Boolean = companionPath ne Nil

    val companionPath: List[String]

    // val companionPath_=(xs: List[String]): Unit //TOASK: What is this
  }

  trait ParamList {
    //val list: List[NamedReference] //TODO
    val isImplicit: Boolean

    //override def toString = list.map(_.title).mkString("(", ",", ")")
  }

  trait Constructors {
    val constructors: List[List[ParamList]]
  }

  trait ReturnValue {
    //val returnValue: Reference //TODO
  }

  //TOASK Pass reflection eachtime
  case class PackageRepresentation(reflect: Reflection)(pid: reflect.kernel.Ref, stats: List[reflect.Tree]) extends Representation with Members {
    import reflect._

    override val (name, path) = {
      val pidSplit = pid.symbol.showCode.split("\\.")
      (pidSplit.last, pidSplit.init.toList)
    }
    override val comments = ???
    override val members = stats.map(convertToRepresentation(reflect)(_))
  }

  //TODO: Use impliedOnly
  case class ImportRepresentation(reflect: Reflection)(impliedOnly: Boolean, expr: reflect.kernel.Term, selectors: List[reflect.ImportSelector]) extends Representation {
    import reflect._

    override val name = selectors.map(_.toString).reduce(_+_)
    override val path = expr.symbol.showCode.split("\\.").toList
    override val comments = ???
  }

  case class ClassRepresenation(reflect: Reflection)(internal: reflect.ClassDef, override val name: String, constr: reflect.kernel.DefDef, parents: List[reflect.TermOrTypeTree], derived: List[reflect.TypeTree], self: Option[reflect.ValDef], body: List[reflect.Statement]) extends Representation with Members with Parents with Modifiers with Companion with ParamList{
    import reflect._
  }

  def convertToRepresentation(reflect: Reflection)(child: reflect.Tree) = {
    import reflect._
    child match {
      case reflect.PackageClause(pid, stats) => PackageRepresentation(reflect)(pid, stats)

      case reflect.Import(impliedOnly, expr, selectors) => ImportRepresentation(reflect)(impliedOnly, expr, selectors)

      case reflect.ClassDef(name, constr, parents, derived, self, body) => ClassRepresenation(reflect)(child, name, constr, parents, derived, self, body)
        //TODO: Generic type
        //TODO: TypeDef
        //TODO: Classes inside class
        // val sign = "class: " + name
        // def iterBody(body: List[reflect.Statement], defdef: List[Container], valdef: List[Container], typedef: List[Container]) : (List[Container], List[Container], List[Container]) = body match {
        //   case Nil => (defdef.reverse, valdef.reverse, typedef.reverse) //TODO: More efficient than reverse?
        //   case (x @ reflect.DefDef(_, _, _, _, _)) :: xs => iterBody(xs, traverse(x)::defdef, valdef, typedef)
        //   case (x @ reflect.ValDef(_, _, _)) :: xs => iterBody(xs, defdef, traverse(x)::valdef, typedef)
        //   //case (x @ reflect.TypeDef())
        //   case x :: xs => iterBody(xs, defdef, valdef, typedef)
        // }
        // val (defdef, valdef, typedef) = iterBody(body, Nil, Nil, Nil)
        // new ClassContainer(sign, defdef, valdef, typedef, extractUserDoc(child.symbol.comment))

      //   case reflect.DefDef(name, typeParams, paramss, tpt, rhs) =>
      //     @tailrec def handleParams(ls: List[List[ValDef]], str: String) : String = ls match {
      //       case Nil => str
      //       case List()::xs => handleParams(xs, str + "()")
      //       case args::xs => handleParams(xs, str + "(" + args.map{case ValDef(vname, vtype, _) => vname + ": " + beautifyType(vtype)}.reduce((x, y) => x + ", " + y) + ")")
      //     }
      //     new DefContainer("def " +
      //       name +
      //       handleParams(paramss, "") +
      //       " : " +
      //       beautifyType(tpt),
      //       extractUserDoc(child.symbol.comment)
      //     )

      //   case reflect.ValDef(name, tpt, rhs) =>
      //     new ValContainer("val " +
      //       name +
      //       " : " +
      //       beautifyType(tpt),
      //       extractUserDoc(child.symbol.comment)
      //     )

      //   case _ => new MissingMatchContainer()
  }}
}