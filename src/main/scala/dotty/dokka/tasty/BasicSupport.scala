package dotty.dokka.tasty

import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import dotty.dokka._

trait BasicSupport:
  self: TastyParser =>
  import reflect._

  extension SymbolOps on (sym: reflect.Symbol):
    def packageName(using ctx: Context): String = 
      if (sym.isPackageDef) sym.fullName 
      else sym.maybeOwner.packageName
    
    def topLevelEntryName(using ctx: Context): Option[String] = if (sym.isPackageDef) None else
      if (sym.owner.isPackageDef) Some(sym.name) else sym.owner.topLevelEntryName

    def getVisibility(): ScalaVisibility = 
      if (sym.flags.is(Flags.Private)) ScalaVisibility.Private
      else if (sym.flags.is(Flags.Protected)) ScalaVisibility.Protected
      else ScalaVisibility.NoModifier

    def getModifier(): ScalaModifier = 
      if (sym.flags.is(Flags.Case)) ScalaModifier.Case
      else if (sym.flags.is(Flags.Abstract)) ScalaModifier.Abstract
      else if (sym.flags.is(Flags.Sealed)) ScalaModifier.Sealed
      else if (sym.flags.is(Flags.Final)) ScalaModifier.Final
      else ScalaModifier.Empty

    // TODO make sure that DRIs are unique plus probably reuse semantic db code?  
    def dri =
      if sym == Symbol.noSymbol then emptyDRI else
        val pointsTo = 
          if (!sym.isTypeDef) PointingToDeclaration.INSTANCE 
          else PointingToGenericParameters(sym.owner.typeMembers.indexOf(sym))

        val method = 
          if (sym.isDefDef) Some(sym) 
          else if (sym.maybeOwner.isDefDef) Some(sym.owner)
          else None 
          
        new DRI(
          sym.packageName,
          sym.topLevelEntryName.orNull, // TODO do we need any of this fields?
          method.map(s => new org.jetbrains.dokka.links.Callable(s.name, null, Nil.asJava)).orNull,
          pointsTo, // TODO different targets?
          s"${sym.show}/${sym.signature.resultSig}/[${sym.signature.paramSigs.mkString("/")}]"
        )

    def documentation(using cxt: reflect.Context) = sym.comment match 
        case Some(comment) => 
            sourceSet.asMap(parseComment(comment, sym.tree))
        case None =>  
            Map.empty.asJava

    def source(using ctx: Context) =
      val path = sym.pos.sourceFile.jpath.toString
      sourceSet.asMap(
        new DocumentableSource:
          override def getPath = path
      )          
  
  private val emptyDRI =  DRI.Companion.getTopLevel