package dotty.tastydoc

import scala.quoted._
import scala.annotation.tailrec
import dotty.tastydoc.comment.Comment
import dotty.tastydoc.references._

object representations extends TastyExtractor {

  trait Representation {
    val name : String
    val path : List[String]
    def comments(packages: Map[String, EmulatedPackageRepresentation], userDocSyntax: String): Option[Comment]
    val parentRepresentation: Option[Representation] //Called simply "parent" in dotty-doc
    val annotations: List[TypeReference]
  }

  trait Parents {
    val parents : List[Reference] //Inheritance similar to supertypes in dotty-doc
  }

  trait Members {
    def members : List[Representation] //Is a def so we can override with either a var or a val (Needed for EmulatedPackage)
  }

  trait Modifiers {
    val modifiers: List[String]
    val privateWithin: Option[Reference]
    val protectedWithin: Option[Reference]

    def isPrivate: Boolean = modifiers.contains("private")
    def isProtected: Boolean = modifiers.contains("protected")
    def isAbstract: Boolean = modifiers.contains("abstract")
  }

  trait Companion {
    val companion: Option[CompanionReference]

    def hasCompanion: Boolean = companion.isDefined //To be consistent with dotty-doc
  }

  trait ParamList {
    val list: List[NamedReference]
    val isImplicit: Boolean
  }

  trait MultipleParamList {
    val paramLists: List[ParamList]
  }

  trait Constructors {
    val constructors: List[DefRepresentation]
  }

  trait ReturnValue {
    val returnValue: Reference
  }

  trait TypeParams {
    val typeParams: List[String]
  }

  /** This contains all the PackageRepresentation representing a single package
   */
  class EmulatedPackageRepresentation(val name: String, val path: List[String])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends Representation with Members {
    override val parentRepresentation = None
    override val annotations = Nil
    override def comments(packages: Map[String, EmulatedPackageRepresentation], userDocSyntax: String) = None
    var packagesMembers: List[PackageRepresentation] = Nil

    //From the outisde, calling members is seemless and appears like calling members on a PackageRepresentation
    override def members = {
      @tailrec
      def noDuplicates(seenPackages: Set[String], members: List[Representation], acc: List[Representation]): (List[Representation], Set[String]) = members match {
        case Nil => (acc, seenPackages)
        case (x: PackageRepresentation)::xs if seenPackages.contains(x.name) => noDuplicates(seenPackages, xs, acc)
        case (x: PackageRepresentation)::xs => noDuplicates(seenPackages + x.name, xs, mutablePackagesMap((x.path :+ x.name).mkString(".")) :: acc)
        case x::xs => noDuplicates(seenPackages, xs, x::acc)
      }

      packagesMembers.foldLeft((List.empty[Representation], Set.empty[String]))((acc, p) => noDuplicates(acc._2, p.members, acc._1))._1
    }
  }

  class PackageRepresentation(using QuoteContext)(internal: qctx.tasty.PackageClause, override val parentRepresentation: Option[Representation])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends Representation with Members {
    import qctx.tasty._

    override val (name, path) = extractPackageNameAndPath(internal.pid.show)
    override val members = internal.stats.map(convertToRepresentation(_, Some(this)))
    override val annotations = extractAnnotations(internal.symbol.annots)

    override def comments(packages: Map[String, EmulatedPackageRepresentation], userDocSyntax: String) = extractComments(internal.symbol.comment, this)(packages, userDocSyntax)
  }

  class ImportRepresentation(using QuoteContext)(internal: qctx.tasty.Import, override val parentRepresentation: Option[Representation])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends Representation {
    import qctx.tasty._

    override val name = if (internal.selectors.size > 1){
        internal.selectors.map(_.toString).mkString("{", ", ", "}")
      } else {
        internal.selectors.head.toString
      }
    override val path = internal.expr.symbol.show.split("\\.").toList
    override val annotations = extractAnnotations(internal.symbol.annots)

    override def comments(packages: Map[String, EmulatedPackageRepresentation], userDocSyntax: String) = extractComments(internal.symbol.comment, this)(packages, userDocSyntax)
  }

  class ClassRepresentation(using QuoteContext)(internal: qctx.tasty.ClassDef, override val parentRepresentation: Option[Representation])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends Representation with Members with Parents with Modifiers with Companion with Constructors with TypeParams {
    import qctx.tasty._

    override val path = extractPath(internal.symbol)
    override val parents = extractParents(internal.parents)
    override val (modifiers, privateWithin, protectedWithin) = extractModifiers(internal.symbol.flags, internal.symbol.privateWithin, internal.symbol.protectedWithin)
    override val constructors =
      (convertToRepresentation(internal.constructor, Some(this)) ::
      (internal.body.flatMap{_ match {
        case d: DefDef => if(d.name == "<init>") Some(d) else None
        case _ => None
        }
      }.map(convertToRepresentation(_, Some(this)))
      )).flatMap{r => r match {
        case r: DefRepresentation => Some(r)
        case _ => None
        }
      }
    override val typeParams = internal.constructor.typeParams.map(x => x.show.stripPrefix("type "))
    override val annotations = extractAnnotations(internal.symbol.annots)
    var knownSubclasses: List[Reference] = Nil

    val (isCase, isTrait, isObject, kind) = extractKind(internal.symbol.flags)

    override val name = internal.name

    override val companion = extractCompanion(
      Some(internal.symbol.companionModule).filter(_.exists), // TODO: refactor later, there is now a NoSymbol
      Some(internal.symbol.companionClass).filter(_.exists), // TODO: refactor later, there is now a NoSymbol
      !isObject
    )
    override val members: List[Representation with Modifiers] = extractClassMembers(internal.body, internal.symbol, Some(this))

    override def comments(packages: Map[String, EmulatedPackageRepresentation], userDocSyntax: String) = extractComments(internal.symbol.comment, this)(packages, userDocSyntax)
  }

  class DefRepresentation(using QuoteContext)(internal: qctx.tasty.DefDef, override val parentRepresentation: Option[Representation])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends Representation with Modifiers with TypeParams with MultipleParamList with ReturnValue {
    import qctx.tasty._

    override val name = internal.name
    override val path = extractPath(internal.symbol)
    override val (modifiers, privateWithin, protectedWithin) = extractModifiers(internal.symbol.flags, internal.symbol.privateWithin, internal.symbol.protectedWithin)
    override val typeParams = internal.typeParams.map(x => x.show.stripPrefix("type "))

    override val paramLists = internal.paramss.map{p =>
      new ParamList {
        override val list = p.map(x => NamedReference(x.name, convertTypeToReference(x.tpt.tpe)))
        override val isImplicit = if(p.nonEmpty) p.head.symbol.flags.is(Flags.Implicit) else false
      }
    }
    override val returnValue = convertTypeToReference(internal.returnTpt.tpe)
    override val annotations = extractAnnotations(internal.symbol.annots)
    override def comments(packages: Map[String, EmulatedPackageRepresentation], userDocSyntax: String) = extractComments(internal.symbol.comment, this)(packages, userDocSyntax)
  }

  class ValRepresentation(using QuoteContext)(internal: qctx.tasty.ValDef, override val parentRepresentation: Option[Representation])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends Representation with Modifiers with ReturnValue {
    import qctx.tasty._

    override val name = internal.name
    override val path = extractPath(internal.symbol)
    override val (modifiers, privateWithin, protectedWithin) = extractModifiers(internal.symbol.flags, internal.symbol.privateWithin, internal.symbol.protectedWithin)
    override val returnValue = convertTypeToReference(internal.tpt.tpe)
    override val annotations = extractAnnotations(internal.symbol.annots)
    val isVar: Boolean = internal.symbol.flags.is(Flags.Mutable)

    override def comments(packages: Map[String, EmulatedPackageRepresentation], userDocSyntax: String) = extractComments(internal.symbol.comment, this)(packages, userDocSyntax)
  }

  class TypeRepresentation(using QuoteContext)(internal: qctx.tasty.TypeDef, override val parentRepresentation: Option[Representation])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends Representation with Modifiers with TypeParams {
    import qctx.tasty._

    override val name = internal.name
    override val path = extractPath(internal.symbol)
    override val (modifiers, privateWithin, protectedWithin) = extractModifiers(internal.symbol.flags, internal.symbol.privateWithin, internal.symbol.protectedWithin)
    override val typeParams = Nil
    override val annotations = extractAnnotations(internal.symbol.annots)
    val alias: Option[Reference] = internal.rhs match{
      case t: TypeBoundsTree => Some(convertTypeToReference(t.tpe))
      case t: TypeTree => Some(convertTypeToReference(t.tpe))
      case _ => None
    }
    override def isAbstract: Boolean = !alias.isDefined
    override def comments(packages: Map[String, EmulatedPackageRepresentation], userDocSyntax: String) = extractComments(internal.symbol.comment, this)(packages, userDocSyntax)
  }

  def convertToRepresentation(using QuoteContext)(tree: qctx.tasty.Tree, parentRepresentation: Option[Representation])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]): Representation = {
    import qctx.tasty._

    tree match {
      case t: PackageClause =>
        val noColorPid = t.pid.symbol.show
        val emulatedPackage = mutablePackagesMap.get(noColorPid) match {
          case Some(x) => x
          case None =>
            val (name, path) = extractPackageNameAndPath(noColorPid)
            val x = new EmulatedPackageRepresentation(name, path)
            mutablePackagesMap += ((noColorPid, x))
            x
        }
        val r = new PackageRepresentation()(t, parentRepresentation)
        emulatedPackage.packagesMembers = r :: emulatedPackage.packagesMembers
        r

      case t: Import => new ImportRepresentation()(t, parentRepresentation)

      case t: ClassDef => new ClassRepresentation()(t, parentRepresentation)

      case t: DefDef => new DefRepresentation()(t, parentRepresentation)

      case t: ValDef => new ValRepresentation()(t, parentRepresentation)

      case t: TypeDef => new TypeRepresentation()(t, parentRepresentation)

      case _ => throw new Exception("Tree match error in conversion to representation. Please open an issue. " + tree)
  }}

  def setSubClasses(mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]): Unit = {
    def innerLogic(representation: Representation): Unit = representation match {
      case r: ClassRepresentation =>
        r.parents.foreach{_ match {
          case ref@TypeReference(label, path, _, _) => mutablePackagesMap.get(path.replaceFirst("/", "").replaceAll("/", ".")) match {
            case Some(p) =>
              p.members.filter(_.name == label).foreach{_ match {
                case parent: ClassRepresentation => parent.knownSubclasses = TypeReference(r.name, r.path.mkString("/", "/", ""), Nil, true) :: parent.knownSubclasses
                case _ =>
              }}
            case None =>
          }
          case _ =>
        }}
      case r: Representation with Members => r.members.foreach(innerLogic)
      case _ =>
    }

    mutablePackagesMap.foreach((_, v) => innerLogic(v))
  }
}
