package dotty.tastydoc

import scala.tasty.Reflection
import dotty.tastydoc.comment.{CommentParser, CommentCleaner, Comment, WikiComment, MarkdownComment}
import dotty.tastydoc.references._
import dotty.tastydoc.representations._

trait TastyExtractor extends TastyTypeConverter with CommentParser with CommentCleaner{
  def extractPath(reflect: Reflection)(symbol: reflect.Symbol) : List[String] = {
    import reflect._

    val pathArray = symbol.show.split("\\.")
    pathArray.view(0, pathArray.length - 1).toList
  }

  def extractModifiers(reflect: Reflection)(flags: reflect.Flags, privateWithin: Option[reflect.Type], protectedWithin: Option[reflect.Type]) : (List[String], Option[Reference], Option[Reference]) = {
    import reflect._

    (((if(flags.is(Flags.Override)) "override" else "") ::
    (if(flags.is(Flags.Private)) "private" else "")::
    (if(flags.is(Flags.Protected)) "protected" else "") ::
    (if(flags.is(Flags.Final)) "final" else "") ::
    (if(flags.is(Flags.Sealed)) "sealed" else "") ::
    (if(flags.is(Flags.Implicit)) "implicit" else "") ::
    (if(flags.is(Flags.Abstract)) "abstract" else "") ::
    (if(flags.is(Flags.Inline)) "inline" else "") ::
    Nil) filter (_ != ""),

    privateWithin match {
      case Some(t) => Some(convertTypeToReference(reflect)(t))
      case None => None
    },
    protectedWithin match {
      case Some(t) => Some(convertTypeToReference(reflect)(t))
      case None => None
    })
  }

  def extractComments(reflect: Reflection)(comment: Option[reflect.Comment], rep: Representation) : Option[Comment] = {
    import reflect._
    comment match {
      case Some(com) =>
        val parsed = parse(Map.empty, clean(com.raw), com.raw)
        if (TastydocConsumer.userDocSyntax == "markdown") {
          Some(MarkdownComment(rep, parsed).comment)
        }else if(TastydocConsumer.userDocSyntax == "wiki"){
          Some(WikiComment(rep, parsed).comment)
        }else{
          Some(WikiComment(rep, parsed).comment)
        }
      case None => None
    }
  }

  def extractClassMembers(reflect: Reflection)(body: List[reflect.Statement], symbol: reflect.ClassDefSymbol, parentRepresentation: Some[Representation]) : List[Representation] = {
    import reflect._
    (body.flatMap{
        case IsDefDef(_) => None //No definitions, they are appended with symbol.methods below
        case x => Some(x)
      }.filter{x => //Filter fields which shouldn't be displayed in the doc
        //!x.symbol.flags.is(Flags.Local) && //Locally defined
        !x.symbol.flags.is(Flags.Private) &&
        !x.symbol.flags.is(Flags.Synthetic) &&
        !x.symbol.flags.is(Flags.Artifact)
      }
      .map(convertToRepresentation(reflect)(_, parentRepresentation)) ++
    symbol.methods.map{x => convertToRepresentation(reflect)(x.tree, parentRepresentation)})
    .sortBy(_.name)
  }

  def extractParents(reflect: Reflection)(parents: List[reflect.Tree]): List[Reference] = {
    import reflect._

    val parentsReferences = parents.map{
      case reflect.IsTypeTree(c) => convertTypeToReference(reflect)(c.tpe)
      case reflect.IsTerm(c) => convertTypeToReference(reflect)(c.tpe)
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
  def extractKind(reflect: Reflection)(flags: reflect.Flags): (Boolean, Boolean, Boolean, String) = { //TOASK All in Classrepresentation?
    import reflect._

    val isCase = flags.is(reflect.Flags.Case)
    val isTrait = flags.is(reflect.Flags.Trait)
    val isObject = flags.is(reflect.Flags.Object)
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

  def extractCompanion(reflect: Reflection)(companionModule: Option[reflect.ValDefSymbol], companionClass: Option[reflect.ClassDefSymbol], companionIsObject: Boolean): Option[CompanionReference] = {
    import reflect._

    if(companionIsObject){
      companionModule match {
        case Some(c) =>
          val path = extractPath(reflect)(c)
           val (_, _, _, kind) = extractKind(reflect)(c.flags)
          Some(CompanionReference(c.name + "$", path.mkString("/", "/", ""), kind))
        case None => None
      }
    }else{
      companionClass match {
        case Some(c) =>
          val path = extractPath(reflect)(c)
          val (_, _, _, kind) = extractKind(reflect)(c.flags)
          Some(CompanionReference(c.name, path.mkString("/", "/", ""), kind))
        case None => None
      }
    }
  }

  def extractAnnotations(reflect: Reflection)(annots: List[reflect.Term]): List[TypeReference] = {
    import reflect._

    def keepAnnot(label: String, link: String): Boolean = {
      !(label == "SourceFile" && link == "/internal") &&
      !(label == "Child" && link == "/internal") //TOASK ok?
    }

    annots.flatMap{a =>
      convertTypeToReference(reflect)(a.tpe) match {
        case ref@TypeReference(label, link, _, _) if keepAnnot(label, link) => Some(ref)
        case _ => None
      }
    }
  }

  def extractPackageNameAndPath(pidShow: String): (String, List[String]) = {
    val pidSplit = representations.removeColorFromType(pidShow).split("\\.")
    (pidSplit.last, pidSplit.init.toList)
  }
}