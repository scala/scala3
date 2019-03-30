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

    // val isPrivate: Boolean = modifiers.contains("private")

    // val isProtected: Boolean = modifiers.contains("protected")
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
  case class PackageRepresentation(reflect: Reflection)(internal: reflect.PackageClause) extends Representation with Members {
    import reflect._

    override val (name, path) = {
      val pidSplit = internal.pid.symbol.showCode.split("\\.")
      (pidSplit.last, pidSplit.init.toList)
    }
    override val comments = ""
    override val members = internal.stats.map(convertToRepresentation(reflect)(_))
  }

  //TODO: Handle impliedOnly
  case class ImportRepresentation(reflect: Reflection)(internal: reflect.Import) extends Representation {
    import reflect._

    override val name = internal.selectors.map(_.toString).reduce(_+_)
    override val path = internal.expr.symbol.showCode.split("\\.").toList
    override val comments = ""
  }

  case class ClassRepresentation(reflect: Reflection)(internal: reflect.ClassDef) extends Representation with Members with Parents with Modifiers with Companion with Constructors {
    import reflect._

    override val name = internal.name
    override val path = Nil
    override val comments = ""
    override val members = internal.body.map(convertToRepresentation(reflect))//TOASK: Override for trait member or just val?
    override val parents = Nil
    override val modifiers = internal.symbol.flags.showCode.replaceAll("\\/\\*|\\*\\/", "").split(" ").toList
    override val companionPath = Nil
    override val constructors = Nil
  }

  case class DebugRepresentation(reflect: Reflection) extends Representation {
    val name = "DEBUG"
    val path = Nil
    val comments = ""
  }

  def convertToRepresentation(reflect: Reflection)(child: reflect.Tree) = {
    import reflect._
    child match {
      case reflect.PackageClause(_) => PackageRepresentation(reflect)(child.asInstanceOf)

      case reflect.Import(_) => ImportRepresentation(reflect)(child.asInstanceOf)

      case reflect.ClassDef(_) => ClassRepresentation(reflect)(child.asInstanceOf) //TOASK: asInstanceOf bad practice?

      case _ => DebugRepresentation(reflect)

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