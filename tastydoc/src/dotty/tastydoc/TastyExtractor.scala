package dotty.tastydoc

import scala.quoted._
import dotty.tastydoc.comment.{CommentParser, CommentCleaner, Comment, WikiComment, MarkdownComment}
import dotty.tastydoc.references._
import dotty.tastydoc.representations._

/** A trait containing useful methods for extracting information from the reflect API */
trait TastyExtractor extends TastyTypeConverter with CommentParser with CommentCleaner{
  def extractPath(using QuoteContext)(symbol: qctx.tasty.Symbol) : List[String] = {
    import qctx.tasty._

    val pathArray = symbol.show.split("\\.") // NOTE: this should print w/o colors, inspect afterwards
    pathArray.iterator.slice(0, pathArray.length - 1).toList
  }

  def extractModifiers(using QuoteContext)(flags: qctx.tasty.Flags, privateWithin: Option[qctx.tasty.Type], protectedWithin: Option[qctx.tasty.Type]) : (List[String], Option[Reference], Option[Reference]) = {
    import qctx.tasty._

    (((if(flags.is(Flags.Override)) "override" else "") ::
    (if(flags.is(Flags.Private)) "private" else "")::
    (if(flags.is(Flags.Protected)) "protected" else "") ::
    (if(flags.is(Flags.Final)) "final" else "") ::
    (if(flags.is(Flags.Sealed)) "sealed" else "") ::
    (if(flags.is(Flags.Implicit)) "implicit" else "") ::
    (if(flags.is(Flags.Abstract)) "abstract" else "") ::
    // (if(flags.is(Flags.AbsOverride)) "absOverride" else "") :: //TOFIX Not visible, fix in Dotty. When fixed, need fix in output as well
    // (if(flags.is(Flags.Deferred)) "deferred" else "") :: //TOFIX Not visible, fix in Dotty. When fixed, need fix in output as well
    (if(flags.is(Flags.Inline)) "inline" else "") ::
    Nil) filter (_ != ""),

    privateWithin match {
      case Some(t) => Some(convertTypeToReference(t))
      case None => None
    },
    protectedWithin match {
      case Some(t) => Some(convertTypeToReference(t))
      case None => None
    })
  }

  def extractComments(using QuoteContext)(comment: Option[qctx.tasty.Comment], rep: Representation) : (Map[String, EmulatedPackageRepresentation], String) => Option[Comment] = {
    comment match {
      case Some(com) =>
        (packages, userDocSyntax) => {
          val parsed = parse(rep, packages, clean(com.raw), com.raw)
          if (userDocSyntax == "markdown") {
            Some(MarkdownComment(rep, parsed, packages).comment)
          }else if(userDocSyntax == "wiki"){
            Some(WikiComment(rep, parsed, packages).comment)
          }else{
            Some(WikiComment(rep, parsed, packages).comment)
          }
        }
      case None => (_, _) => None
    }
  }

  def extractClassMembers(using QuoteContext)(body: List[qctx.tasty.Statement], symbol: qctx.tasty.Symbol, parentRepresentation: Some[Representation])(using mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) : List[Representation with Modifiers] = {
    import qctx.tasty._

    /** Filter fields which shouldn't be displayed in the doc
     */
    def filterSymbol(symbol: Symbol): Boolean = {
      val ownerPath = extractPath(symbol.owner)

      !symbol.flags.is(Flags.Synthetic) &&
      !symbol.flags.is(Flags.Artifact) &&
      !symbol.flags.is(Flags.StableRealizable) && //Remove val generated for object definitions inside classes
      !symbol.name.contains("$default$") &&//Remove artifact methods generated for methods with default parameters
      !(symbol.owner.name == "Object" && ownerPath == List("java", "lang")) &&
      !(symbol.owner.name == "Any" && ownerPath == List("scala"))
    }

    (body.flatMap {
        case _: DefDef => None //No definitions, they are appended with symbol.methods below
        case _: ValDef => None //No val/var, they are appended with symbol.fields below
        case _: Inlined => None //Inlined aren't desirable members
        case x => Some(x)
      }.filter(x => filterSymbol(x.symbol)).map(convertToRepresentation(_, parentRepresentation)) ++
    symbol.methods.filter(x => filterSymbol(x)).map{x => convertToRepresentation(x.tree, parentRepresentation)} ++
    symbol.fields.filter { x =>
      filterSymbol(x)
    }.flatMap {
      case x if x.isValDef => Some(x)
      // case qctx.tasty.IsValDefSymbol(x) => Some(x)
      case _ => None
    }.map { x =>
      convertToRepresentation(x.tree, parentRepresentation)
    }
    )
    .flatMap{
      case r: Representation with Modifiers => Some(r)
      case _ => None
    }
    .sortBy(_.name)
  }

  def extractParents(using QuoteContext)(parents: List[qctx.tasty.Tree]): List[Reference] = {
    import qctx.tasty._

    val parentsReferences = parents.map{
      case c: TypeTree => convertTypeToReference(c.tpe)
      case c: Term => convertTypeToReference(c.tpe)
      case _ => throw Exception("Unhandeld case in parents. Please open an issue.")
    }

    parentsReferences.filter{_ match{
      case TypeReference("Object", "/lang", _, _) | TypeReference("Object", "/java/lang", _, _) => false
      case _ => true
    }}
  }

  /** The kind of a ClassDef can be one of the following: class, case class, object, case object, trait
  *
  * @return (is case, is a trait, is an object, the kind as a String)
  */
  def extractKind(using QuoteContext)(flags: qctx.tasty.Flags): (Boolean, Boolean, Boolean, String) = {
    import qctx.tasty._

    val isCase = flags.is(Flags.Case)
    val isTrait = flags.is(Flags.Trait)
    val isObject = flags.is(Flags.Object)
    val kind = {
      if(isTrait){
        "trait"
      }else{
        (if(isCase){
          "case "
        }else{
          ""
        }) +
        (if(isObject){
          "object"
        }else{
          "class"
        })
      }
    }
    (isCase, isTrait, isObject, kind)
  }

  def extractCompanion(using QuoteContext)(companionModule: Option[qctx.tasty.Symbol], companionClass: Option[qctx.tasty.Symbol], companionIsObject: Boolean): Option[CompanionReference] = {
    import qctx.tasty._

    if(companionIsObject){
      companionModule match {
        case Some(c) =>
          val path = extractPath(c)
           val (_, _, _, kind) = extractKind(c.flags)
          Some(CompanionReference(c.name + "$", path.mkString("/", "/", ""), kind))
        case None => None
      }
    }else{
      companionClass match {
        case Some(c) =>
          val path = extractPath(c)
          val (_, _, _, kind) = extractKind(c.flags)
          Some(CompanionReference(c.name, path.mkString("/", "/", ""), kind))
        case None => None
      }
    }
  }

  def extractAnnotations(using QuoteContext)(annots: List[qctx.tasty.Term]): List[TypeReference] = {
    import qctx.tasty._

    def keepAnnot(label: String, link: String): Boolean = {
      !(label == "SourceFile" && link == "/internal") &&
      !(label == "Child" && link == "/internal")
    }

    annots.flatMap{a =>
      convertTypeToReference(a.tpe) match {
        case ref@TypeReference(label, link, _, _) if keepAnnot(label, link) => Some(ref)
        case _ => None
      }
    }
  }

  def extractPackageNameAndPath(pidShowNoColor: String): (String, List[String]) = {
    val pidSplit = pidShowNoColor.split("\\.")
    (pidSplit.last, pidSplit.init.toList)
  }
}
