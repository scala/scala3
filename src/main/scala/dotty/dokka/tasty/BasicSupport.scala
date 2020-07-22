package dotty.dokka.tasty

import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._

trait BasicSupport:
  self: TastyParser =>
  import reflect._

  extension SymbolOps on (sym: reflect.Symbol):
    def packageName(using ctx: Context): String = 
      if (sym.isPackageDef) sym.fullName 
      else sym.maybeOwner.packageName
    
    def topLevelEntryName(using ctx: Context): Option[String] = if (sym.isPackageDef) None else
      if (sym.owner.isPackageDef) Some(sym.name) else sym.owner.topLevelEntryName

    def dri = asDRI(sym)
    def declarationDri = asDRI(sym, true)   

    def documentation(using cxt: reflect.Context) = sym.comment match 
        case Some(comment) => 
            sourceSet.asMap(parseComment(comment, sym.tree))
        case None =>  
            Map.empty.asJava

    def source(using ctx: Context) = sourceSet.asMap(getSource(sym)) 
    
    def dokkaType(using cxt: reflect.Context): Bound =  // TODO render primitives better?
      // TODO support varags
      val params = sym.typeMembers.map(_.dokkaType)
      println(s"${sym.show} -> ${sym.dri}")
      new org.jetbrains.dokka.model.TypeConstructor(sym.dri, params.asJava, FunctionModifiers.NONE)
    

  val emptyDRI =  DRI.Companion.getTopLevel

    // TODO add support for type aliases!
  def asDRI(symbol: reflect.Symbol, declaration: Boolean = false): DRI = 
    if symbol == Symbol.noSymbol then emptyDRI else
      val pointsTo = 
        if (!symbol.isTypeDef || declaration) PointingToDeclaration.INSTANCE 
        else PointingToGenericParameters(symbol.owner.typeMembers.indexOf(symbol))

      val method = 
        if (symbol.isDefDef) Some(symbol) 
        else if (symbol.maybeOwner.isDefDef) Some(symbol.owner)
        else None 

      new DRI(
        symbol.packageName,
        symbol.topLevelEntryName.orNull, // TODO do we need any of this fields?
        method.map(s => new org.jetbrains.dokka.links.Callable(s.name, null, Nil.asJava)).orNull,
        pointsTo, // TODO different targets?
        symbol.show
      )

  def getSource(symbol: reflect.Symbol)(using ctx: Context): DocumentableSource = 
    val path = symbol.pos.sourceFile.jpath.toString
    new DocumentableSource:
      override def getPath = path 