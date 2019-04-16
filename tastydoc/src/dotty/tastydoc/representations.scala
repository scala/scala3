package dotty.tastydoc

import scala.tasty.Reflection
import dotty.tastydoc.comment.{CommentParser, CommentCleaner, Comment, WikiComment, MarkdownComment}

object representations extends CommentParser with CommentCleaner {

  //TOASK
  //- reflect as implicit?

  //Global TODO
  //- Make sure no Entity is left

  def removeColorFromType(tpe: String) : String = { //TODO: This a workaround, fix this
    tpe.replaceAll("\u001B\\[[;\\d]*m", "")
  }

  case class Ref(title: String, tpe: String) //TODO: This is a workaround

  trait Representation {
    val name : String
    val path : List[String]
    val comments: Option[Comment]
    val parent = None //TODO: Temporary fix for HTMLParsers
  }

  trait Parents {
    val parent : Option[String]
    val parents : List[String]
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

  trait Annotations {
    val annotations: List[String]
  }

  private def extractModifiers(reflect: Reflection)(flags: reflect.Flags) : List[String] = {
    import reflect._

    ((if(flags.is(Flags.Override)) "override" else "") ::
    (if(flags.is(Flags.Private)) "private" else "") ::
    (if(flags.is(Flags.Protected)) "protected" else "") ::
    (if(flags.is(Flags.Final)) "final" else "") ::
    (if(flags.is(Flags.Sealed)) "sealed" else "") ::
    (if(flags.is(Flags.Implicit)) "implicit" else "") ::
    (if(flags.is(Flags.Abstract)) "abstract" else "") ::
    Nil) filter (_ != "")
  }

  private def extractComments(reflect: Reflection)(comment: Option[reflect.Comment], rep: Representation) : Option[Comment] = {
    import reflect._
    comment match {
      case Some(com) =>
        val parsed = parse(Map.empty, clean(com.raw), com.raw)
        if (true) //TODO
          Some(WikiComment(rep, parsed).comment)
        else
          Some(MarkdownComment(rep, parsed).comment)
      case None => None
    }
  }

  private def extractMembers(reflect: Reflection)(body: List[reflect.Statement]) : List[Representation] = {
    import reflect._
    body.filter{x => //Filter fields which shouldn't be displayed in the doc
        !removeColorFromType(x.showCode).contains("def this") && //No constructor
        !x.symbol.flags.is(Flags.Local) //Locally defined
      }
      .map(convertToRepresentation(reflect))
  }

  class PackageRepresentation(reflect: Reflection, internal: reflect.PackageClause) extends Representation with Members {
    import reflect._

    override val (name, path) = {
      val pidSplit = internal.pid.symbol.showCode.split("\\.")
      (pidSplit.last, pidSplit.init.toList)
    }
    override val members = internal.stats.map(convertToRepresentation(reflect)(_))
    override val comments = extractComments(reflect)(internal.symbol.comment, this)
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
    override val comments = extractComments(reflect)(internal.symbol.comment, this)
  }

  class ClassRepresentation(reflect: Reflection, internal: reflect.ClassDef) extends Representation with Members with Parents with Modifiers with Companion with Constructors with TypeParams with Annotations {
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val members = extractMembers(reflect)(internal.body)
    override val parent = None
    override val parents = internal.parents.map(x => removeColorFromType(x.showCode))
    override val modifiers = extractModifiers(reflect)(internal.symbol.flags)
    override val companionPath = internal.symbol.companionClass match { //TOASK: Right way?
      case Some(_) => path.init ++ List(name)
      case None => Nil
    }
    override val constructors = (internal.constructor :: (internal.body
      .filter(x => removeColorFromType(x.showCode).contains("def this"))
      .flatMap{x => x match {
        case IsDefDef(d@reflect.DefDef(_)) => Some(d)
        case _ => None
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
    override val typeParams = internal.constructor.typeParams.map(x => removeColorFromType(x.showCode).stripPrefix("type "))
    override val annotations = Nil

    override val comments = extractComments(reflect)(internal.symbol.comment, this)
  }

  class ObjectRepresentation(reflect: Reflection, internal: reflect.ClassDef) extends Representation with Modifiers with Members with Companion with Annotations {
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val modifiers = extractModifiers(reflect)(internal.symbol.flags)
    override val members = extractMembers(reflect)(internal.body)
    override val companionPath = internal.symbol.companionClass match { //TOASK: Right way?
      case Some(_) => path.init ++ List(name)
      case None => Nil
    }
    override val annotations = Nil
    override val comments = extractComments(reflect)(internal.symbol.comment, this)
  }

  class DefRepresentation(reflect: Reflection, internal: reflect.DefDef) extends Representation with Parents with Modifiers with TypeParams with MultipleParamList with ReturnValue with Annotations{
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val parent = None
    override val parents = Nil
    override val modifiers = extractModifiers(reflect)(internal.symbol.flags)
    override val typeParams = internal.typeParams.map(x => removeColorFromType(x.showCode).stripPrefix("type "))

    override val paramLists = internal.paramss.map{p =>
      new ParamList {
        override val list = p.map(x => Ref(x.name, removeColorFromType(x.tpt.tpe.showCode)))
        override val isImplicit = if(p.size > 1) p.tail.head.symbol.flags.show.contains("Flags.Implicit") else false //TODO: Verfiy this
      }
    }
    override val returnValue = removeColorFromType(internal.returnTpt.tpe.showCode)
    override val annotations = Nil
    override val comments = extractComments(reflect)(internal.symbol.comment, this)
  }

  class ValRepresentation(reflect: Reflection, internal: reflect.ValDef) extends Representation with Parents with Modifiers with ReturnValue with Annotations {
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val parent = None
    override val parents = Nil
    override val modifiers = extractModifiers(reflect)(internal.symbol.flags)
    override val returnValue = removeColorFromType(internal.tpt.tpe.showCode)
    override val annotations = Nil
    override val comments = extractComments(reflect)(internal.symbol.comment, this)
  }

  class TypeRepresentation(reflect: Reflection, internal: reflect.TypeDef) extends Representation with Modifiers with TypeParams with Annotations {
    import reflect._

    override val name = internal.name
    override val path = internal.symbol.showCode.split("\\.").toList
    override val modifiers = extractModifiers(reflect)(internal.symbol.flags)
    override val typeParams = Nil
    override val annotations = Nil
    override val comments = extractComments(reflect)(internal.symbol.comment, this)
  }

  class DebugRepresentation extends Representation {
    val name = "DEBUG"
    val path = Nil
    val comments = None
  }

  def convertToRepresentation(reflect: Reflection)(tree: reflect.Tree) = {
    import reflect._
    tree match {
      case IsPackageClause(t@reflect.PackageClause(_)) => new PackageRepresentation(reflect, t)

      case IsImport(t@reflect.Import(_)) => new ImportRepresentation(reflect, t)

      case IsClassDef(t@reflect.ClassDef(_)) =>
        if(t.symbol.flags.is(Flags.Object)){
          new ObjectRepresentation(reflect, t)
        }else{
          new ClassRepresentation(reflect, t)
        }
      case IsDefDef(t@reflect.DefDef(_)) => new DefRepresentation(reflect, t)

      case IsValDef(t@reflect.ValDef(_)) => new ValRepresentation(reflect, t) //TODO: contains object too, separate from Val

      case IsTypeDef(t@reflect.TypeDef(_)) => new TypeRepresentation(reflect, t)

      case _ => new DebugRepresentation()
  }}
}