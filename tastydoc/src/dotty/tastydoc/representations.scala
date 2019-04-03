package dotty.tastydoc

import scala.tasty.Reflection

object representations {

  def removeColorFromType(tpe: String) : String = { //TODO: This a workaround, fix this
    tpe.replaceAll("\u001B\\[[;\\d]*m", "")
  }

  case class Ref(title: String, tpe: String) //TODO: This is a workaround

  trait Representation {
    val name : String
    val path : List[String]
    val comments: String
  }

  trait Parents {
    val parent : Option[Representation]
    val parents : List[Representation]
  }

  trait Members {
    val members : List[Representation]
  }

  trait Modifiers {
    val modifiers: List[String]

    def isPrivate: Boolean = modifiers.contains("private")

    def isProtected: Boolean = modifiers.contains("protected")
  }

  trait Companion {
    def hasCompanion: Boolean = companionPath ne Nil

    val companionPath: List[String]

    // val companionPath_=(xs: List[String]): Unit //TOASK: What is this
  }

  trait ParamList {
    val list: List[Ref] //TODO: Use Reference, currently using Ref workaround
    val isImplicit: Boolean

    //override def toString = list.map(_.title).mkString("(", ",", ")")
  }

  trait MultipleParamList {
    val paramLists: List[ParamList]
  }

  trait Constructors {
    val constructors: List[MultipleParamList]
  }

  trait ReturnValue {
    val returnValue: String //TODO: Use Reference, currently using String workaround
  }

  trait TypeParams {
    val typeParams: List[String]
  }

  class PackageRepresentation(reflect: Reflection, internal: reflect.PackageClause) extends Representation with Members {
    import reflect._

    override val (name, path) = {
      val pidSplit = internal.pid.symbol.showCode.split("\\.")
      (pidSplit.last, pidSplit.init.toList)
    }
    override val comments = "Comments placeholder"
    override val members = internal.stats.map(convertToRepresentation(reflect)(_))
  }

  //TODO: Handle impliedOnly
  class ImportRepresentation(reflect: Reflection, internal: reflect.Import) extends Representation {
    import reflect._

    override val name = if (internal.selectors.size > 1){
        internal.selectors.map(_.toString).mkString("{", ", ", "}")
      } else {
        internal.selectors.head.toString
      }
    override val path = internal.expr.symbol.showCode.split("\\.").toList
    override val comments = "Comments placeholder"
  }

  class ClassRepresentation(reflect: Reflection, internal: reflect.ClassDef) extends Representation with Members with Parents with Modifiers with Companion with Constructors with TypeParams {
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val comments = "Comments placeholder"
    override val members = internal.body.map(convertToRepresentation(reflect))//TOASK: Override for trait member or just val?
    override val parent = None //TOASK: Why 2 and how to do it
    override val parents = Nil
    override val modifiers = internal.symbol.flags.showCode.replaceAll("\\/\\*|\\*\\/", "").split(" ").filter(_!="").toList
    override val companionPath = internal.symbol.companionClass match { //TOASK: Right way?
      case Some(_) => path.init ++ List(name + "$")
      case None => Nil
      }
    override val constructors = Nil
    override val typeParams = Nil
  }

  class DefRepresentation(reflect: Reflection, internal: reflect.DefDef) extends Representation with Parents with Modifiers with TypeParams with MultipleParamList with ReturnValue{
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val comments = "Comments placeholder"
    override val parent = None
    override val parents = Nil
    override val modifiers = internal.symbol.flags.showCode.replaceAll("\\/\\*|\\*\\/", "").split(" ").filter(_!="").toList
    override val typeParams = Nil
    override val paramLists = internal.paramss.map{p =>
      new ParamList {
        override val list = p.map(x => Ref(x.name, removeColorFromType(x.tpt.tpe.showCode)))
        override val isImplicit = if(p.size > 1) p.tail.head.symbol.flags.show.contains("Flags.Implicit") else false //TODO: Verfiy this
      }
    }
    override val returnValue = removeColorFromType(internal.returnTpt.tpe.showCode)
  }

  class ValRepresentation(reflect: Reflection, internal: reflect.ValDef) extends Representation with Parents with Modifiers with ReturnValue {
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val comments = "Comments placeholder"
    override val parent = None
    override val parents = Nil
    override val modifiers = internal.symbol.flags.showCode.replaceAll("\\/\\*|\\*\\/", "").split(" ").filter(_!="").toList
    override val returnValue = removeColorFromType(internal.tpt.tpe.showCode)
  }

  class TypeRepresentation(reflect: Reflection, internal: reflect.TypeDef) extends Representation with Modifiers with TypeParams {
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val comments = "Comments placeholder"
    override val modifiers = internal.symbol.flags.showCode.replaceAll("\\/\\*|\\*\\/", "").split(" ").filter(_!="").toList
    override val typeParams = Nil
  }

  class DebugRepresentation(reflect: Reflection) extends Representation {
    val name = "DEBUG"
    val path = Nil
    val comments = ""
  }

  def convertToRepresentation(reflect: Reflection)(child: reflect.Tree) = {
    import reflect._
    child match {
      case reflect.PackageClause(_) => new PackageRepresentation(reflect, child.asInstanceOf)

      case reflect.Import(_) => new ImportRepresentation(reflect, child.asInstanceOf)

      case reflect.ClassDef(_) => new ClassRepresentation(reflect, child.asInstanceOf)

      case reflect.DefDef(_) => new DefRepresentation(reflect, child.asInstanceOf)

      case reflect.ValDef(_) => new ValRepresentation(reflect, child.asInstanceOf) //TODO: contains object too, separate from Val

      case reflect.TypeDef(_) => new TypeRepresentation(reflect, child.asInstanceOf)

      case _ => new DebugRepresentation(reflect)

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