package dotty.dokka.tasty

import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.DocumentableSource

trait BasicSupport:
  self: TastyParser =>
  import reflect._

  extension SymbolOps on (sym: reflect.Symbol):
    def packageName(using ctx: Context): String = 
      if (sym.isPackageDef) sym.fullName 
      else sym.owner.packageName
    
    def topLevelEntryName(using ctx: Context): Option[String] = if (sym.isPackageDef) None else
      if (sym.owner.isPackageDef) Some(sym.name) else sym.owner.topLevelEntryName

    def dri = asDRI(sym)  

  def asDRI(symbol: reflect.Symbol): DRI = new DRI(
    symbol.packageName,
    symbol.topLevelEntryName.orNull, // TODO do we need any of this fields?
    null, // TODO search for callable here?
    PointingToDeclaration.INSTANCE, // TODO different targets?
    symbol.show
  )

  def getSource(symbol: reflect.Symbol)(using ctx: Context): DocumentableSource = 
    val path = symbol.pos.sourceFile.jpath.toString
    new DocumentableSource:
      override def getPath = path 