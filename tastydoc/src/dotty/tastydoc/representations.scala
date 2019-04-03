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

    // val companionPath_=(xs: List[String]): Unit
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

    //println(internal.body.filter(x => removeColorFromType(x.showCode).contains("def this")).map(_.showCode))


    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val comments = "Comments placeholder"
    override val members = internal.body
      .filter{x => //Filter fields which shouldn't be displayed in the doc
        !removeColorFromType(x.showCode).contains("def this") //No constructor
      }
      .map(convertToRepresentation(reflect))
    override val parent = None
    override val parents = Nil
    override val modifiers = internal.symbol.flags.showCode.replaceAll("\\/\\*|\\*\\/", "").split(" ").filter(_!="").toList
    override val companionPath = internal.symbol.companionClass match { //TOASK: Right way?
      case Some(_) => path.init ++ List(name + "$")
      case None => Nil
      }
    override val constructors = (internal.constructor :: (internal.body
      .filter(x => removeColorFromType(x.showCode).contains("def this"))
      .map{x => x match {
        case IsDefDef(d@reflect.DefDef(_)) => d //TOASK Not safe?
        }
    }))
    .map{x =>
      new MultipleParamList {
        override val paramLists = x.paramss.map{p =>
          new ParamList {
            override val list = p.map(x => Ref(x.name, removeColorFromType(x.tpt.tpe.showCode)))
            override val isImplicit = if(p.size > 1) p.tail.head.symbol.flags.show.contains("Flags.Implicit") else false //TODO: Verfiy this
          }
        }
      }
    }
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

  def convertToRepresentation(reflect: Reflection)(tree: reflect.Tree) = {
    import reflect._
    tree match {
      case IsPackageClause(t@reflect.PackageClause(_)) => new PackageRepresentation(reflect, t)

      case IsImport(t@reflect.Import(_)) => new ImportRepresentation(reflect, t)

      case IsClassDef(t@reflect.ClassDef(_)) => new ClassRepresentation(reflect, t)

      case IsDefDef(t@reflect.DefDef(_)) => new DefRepresentation(reflect, t)

      case IsValDef(t@reflect.ValDef(_)) => new ValRepresentation(reflect, t) //TODO: contains object too, separate from Val

      case IsTypeDef(t@reflect.TypeDef(_)) => new TypeRepresentation(reflect, t)

      case _ => new DebugRepresentation(reflect)
  }}
}