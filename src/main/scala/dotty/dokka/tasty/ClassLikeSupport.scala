package dotty.dokka.tasty

import org.jetbrains.dokka.model.{TypeConstructor => DTypeConstructor, _}
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import dotty.dokka._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions
import dotty.dokka.model.api._
import dotty.dokka.model.api.Modifier
import dotty.dokka.model.api.Kind
import dotty.dokka.model.api.ImplicitConversion

trait ClassLikeSupport:
  self: TastyParser =>
  import reflect._

  private val placeholderVisibility = Map(sourceSet.getSourceSet -> KotlinVisibility.Public.INSTANCE).asJava
  private val placeholderModifier = Map(sourceSet.getSourceSet -> KotlinModifier.Empty.INSTANCE).asJava

  private def kindForClasslike(sym: Symbol): Kind = 
        if sym.flags.is(Flags.Object) then Kind.Object
        else if sym.flags.is(Flags.Trait) then Kind.Trait
        else if sym.flags.is(Flags.Enum) then Kind.Enum
        else Kind.Class

  object DClass:
    def apply[T >: DClass](classDef: ClassDef)(
      dri: DRI = classDef.symbol.dri,
      name: String = classDef.name,
      modifiers: Seq[Modifier] = classDef.symbol.getExtraModifiers(),
      constructors: List[DFunction] = classDef.getConstructors.map(parseMethod(_)),
      sources: Map[DokkaConfiguration$DokkaSourceSet, DocumentableSource] = classDef.symbol.source,
      supertypes: Map[DokkaConfiguration$DokkaSourceSet, List[TypeConstructorWithKind]] = Map.empty,
      documentation: Map[DokkaConfiguration$DokkaSourceSet, DocumentationNode] = classDef.symbol.documentation,
    ): DClass = 
      val supertypes = classDef.getParents.map {tree =>
        val actualSymbol = if tree.symbol.isClassConstructor then tree.symbol.owner else tree.symbol
        LinkToType(tree.dokkaType.asSignature, actualSymbol.dri, kindForClasslike(actualSymbol))
      }

      val kind = kindForClasslike(classDef.symbol)
      new DClass(
          dri,
          name,
          constructors.asJava,
          Nil.asJava,
          Nil.asJava,
          Nil.asJava,
          sources.asJava,
          placeholderVisibility,
          null,
          /*generics =*/classDef.getTypeParams.map(parseTypeArgument).asJava,
          Map.empty.asJava,
          documentation.asJava,
          null,
          placeholderModifier,
          inspector.sourceSet.toSet,
          PropertyContainer.Companion.empty()
            .plus(ClasslikeExtension(
              classDef.getConstructorMethod,
              classDef.getCompanion
            ))
            .plus(CompositeMemberExtension(classDef.extractMembers, supertypes, Nil))
            .plus(MemberExtension(classDef.symbol.getVisibility(), modifiers,kind , classDef.symbol.getAnnotations()))
      )

    def parseForSignatureOnly(classDef: ClassDef)(
      dri: DRI = classDef.symbol.dri,
      name: String = classDef.name,
      sources: Map[DokkaConfiguration$DokkaSourceSet, DocumentableSource] = classDef.symbol.source,
      supertypes: Map[DokkaConfiguration$DokkaSourceSet, List[TypeConstructorWithKind]] = Map.empty,
      documentation: Map[DokkaConfiguration$DokkaSourceSet, DocumentationNode] = classDef.symbol.documentation,
      modifiers: Seq[Modifier] = classDef.symbol.getExtraModifiers(),
    ): DClass = new DClass(
        dri,
        name,
        JList(),
        JList(),
        JList(),
        JList(),
        sources.asJava,
        placeholderVisibility,
        null,
        /*generics =*/classDef.getTypeParams.map(parseTypeArgument).asJava,
        supertypes.map{case (key,value) => (key, value.asJava)}.asJava,
        documentation.asJava,
        null,
        placeholderModifier,
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty()
          .plus(ClasslikeExtension(
            classDef.getConstructorMethod,
            None,
          ))
          .plus(MemberExtension(classDef.symbol.getVisibility(), modifiers, kindForClasslike(classDef.symbol), classDef.symbol.getAnnotations()))
    )
    
  private val conversionSymbol = Symbol.requiredClass("scala.Conversion")

  def extractImplicitConversion(tpe: Type): Option[ImplicitConversion] =
      if tpe.derivesFrom(conversionSymbol) then None
      else tpe.baseType(conversionSymbol) match
        case AppliedType(tpe, List(from: Type, to: Type)) =>
          Some(ImplicitConversion(from.typeSymbol.dri, to.typeSymbol.dri))
        case _ =>
          None

  private def parseMember(s: Tree): Option[Member] = processTreeOpt(s)(s match 
      case dd: DefDef if !dd.symbol.isHiddenByVisibility && !dd.symbol.isSyntheticFunc && dd.symbol.isExtensionMethod =>
        dd.symbol.extendedSymbol.map { extSym => 
          val target = ExtensionTarget(extSym.symbol.name, extSym.tpt.dokkaType.asSignature, extSym.tpt.symbol.dri)
          parseMethod(dd.symbol, kind = Kind.Extension(target))
        }
    
      case dd: DefDef if !dd.symbol.isHiddenByVisibility && dd.symbol.isGiven =>
        Some(parseMethod(dd.symbol, kind = Kind.Given(getGivenInstance(dd).map(_.asSignature), None))) // TODO check given methods?

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && !dd.symbol.isGiven && !dd.symbol.isSyntheticFunc && !dd.symbol.isExtensionMethod =>
        Some(parseMethod(dd.symbol))

      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && (!td.symbol.flags.is(Flags.Case) || !td.symbol.flags.is(Flags.Enum)) => 
        Some(parseTypeDef(td))  

      case vd: ValDef if !isSyntheticField(vd.symbol) && (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum)) => 
        Some(parseValDef(vd))

      case c: ClassDef if c.symbol.shouldDocumentClasslike &&  !c.symbol.isGiven => 
        Some(parseClasslike(c))

      case _ => None
  )

  extension (c: ClassDef):
    def membersToDocument = c.body.filterNot(_.symbol.isHiddenByVisibility)

    def getNonTrivialInheritedMemberTrees = 
      c.symbol.getAllMembers.filterNot(s => s.isHiddenByVisibility || s.maybeOwner == c.symbol)
        .filter(s => s.maybeOwner != defn.ObjectClass && s.maybeOwner != defn.AnyClass)
        .map(_.tree)

    def extractMembers: Seq[Member] = {
      val inherited = c.getNonTrivialInheritedMemberTrees.collect { 
          case dd: DefDef if !dd.symbol.isClassConstructor && !(dd.symbol.isSuperBridgeMethod || dd.symbol.isDefaultHelperMethod) => dd 
          case other if !other.isInstanceOf[DefDef] => other
        }

      c.membersToDocument.flatMap(parseMember) ++ 
        inherited.flatMap(s => parseMember(s).map(_.withOrigin(Origin.InheritedFrom(s.symbol.owner.name, s.symbol.owner.dri))))
    }

    def getParents: List[Tree] =
      for
        parentTree <- c.parents if isValidPos(parentTree.pos)  // We assume here that order is correct
        parentSymbol = if parentTree.symbol.isClassConstructor then parentTree.symbol.owner else parentTree.symbol
        if parentSymbol != defn.ObjectClass && parentSymbol != defn.AnyClass
      yield parentTree
      

    def getConstructors: List[Symbol] = membersToDocument.collect {
      case d: DefDef if d.symbol.isClassConstructor && c.constructor.symbol != d.symbol => d.symbol
    }.toList

    def getParameterModifier(parameter: Symbol): String =
      val fieldSymbol = c.symbol.field(parameter.name)
      if fieldSymbol.flags.is(Flags.Mutable) then "var "
      else if fieldSymbol.flags.is(Flags.ParamAccessor) && !c.symbol.flags.is(Flags.Case) && !fieldSymbol.flags.is(Flags.Private) then "val "
      else ""

    def getTypeParams: List[TypeDef] = c.body.collect { case targ: TypeDef => targ  }.filter(_.symbol.isTypeParam)

    def getCompanion: Option[DRI] = c.symbol.getCompanionSymbol
      .filter(!_.flags.is(Flags.Synthetic))
      .filterNot(_.isHiddenByVisibility)
      .map(_.dri)

    def getConstructorMethod: Option[DFunction] =
      Some(c.constructor.symbol).filter(_.exists).filterNot(_.isHiddenByVisibility).map( d =>
        parseMethod(d, constructorWithoutParamLists(c), s => c.getParameterModifier(s))
      )

  def parseClasslike(classDef: reflect.ClassDef, forSignature: Boolean = false)(using ctx: Context): DClass = classDef match
    case c: ClassDef if classDef.symbol.flags.is(Flags.Object) => parseObject(c, forSignature)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) => parseEnum(c, forSignature)
    case clazz => if forSignature then DClass.parseForSignatureOnly(classDef)() else DClass(classDef)()

  def parseObject(classDef: reflect.ClassDef, forSignature: Boolean = false)(using ctx: Context): DClass =
    // All objects are final so we do not need final modifer!
    val modifiers = classDef.symbol.getExtraModifiers().filter(_ != Modifier.Final)

    if forSignature then DClass.parseForSignatureOnly(classDef)(
      name = classDef.name.stripSuffix("$"),
      modifiers = modifiers,
    ) 
    else DClass(classDef)(
      name = classDef.name.stripSuffix("$"),
      modifiers = modifiers,
    )

    // TODO check withNewExtras?
  def parseEnum(classDef: reflect.ClassDef, forSignature: Boolean = false)(using ctx: Context): DClass =
    val extraModifiers = classDef.symbol.getExtraModifiers().filter(_ != Modifier.Sealed).filter(_ != Modifier.Abstract)
    val companion = classDef.symbol.getCompanionSymbol.map(_.tree.asInstanceOf[ClassDef]).get

    val enumVals = companion.membersToDocument.collect {
      case vd: ValDef if !isSyntheticField(vd.symbol, classDef) && vd.symbol.flags.is(Flags.Enum) && vd.symbol.flags.is(Flags.Case) => vd
    }.toList.map(parseValDef(_))

    val enumTypes = companion.membersToDocument.collect {
      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(parseTypeDef)

    val enumNested = companion.membersToDocument.collect {
      case c: ClassDef if c.symbol.flags.is(Flags.Case) && c.symbol.flags.is(Flags.Enum) => processTree(c)(parseClasslike(c))
    }.flatten

    val classlikie = 
      if forSignature then DClass.parseForSignatureOnly(classDef)(modifiers = extraModifiers)
      else DClass(classDef)(modifiers = extraModifiers)

    classlikie.withNewMembers((enumVals ++ enumTypes ++ enumNested).map(_.withKind(Kind.EnumCase))).asInstanceOf[DClass]
   
  def parseMethod(
      methodSymbol: Symbol,
      emptyParamsList: Boolean = false,
      paramPrefix: Symbol => String = _ => "",
      kind: Kind = Kind.Def
    ): DFunction =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = if emptyParamsList then Nil else method.paramss
    val genericTypes = if (methodSymbol.isClassConstructor) Nil else method.typeParams

    val methodKind = 
      if methodSymbol.isClassConstructor then Kind.Constructor 
      else if methodSymbol.flags.is(Flags.Implicit) then extractImplicitConversion(method.returnTpt.tpe) match
        case Some(conversion) if paramLists.size == 0 || (paramLists.size == 1 && paramLists.head.size == 0) => 
          Kind.Implicit(Kind.Def, Some(conversion))
        case _ => 
          Kind.Implicit(Kind.Def, None)
      else kind
    
    val name = methodKind match
      case Kind.Constructor => "this"
      case Kind.Given(_, _) => methodSymbol.name.stripPrefix("given_")
      case Kind.Extension(_) => methodSymbol.name.stripPrefix("extension_")
      case _ => methodSymbol.name

    new DFunction(
      methodSymbol.dri,
      name,
      /*isConstructor =*/ methodSymbol.isClassConstructor,
      /*parameters =*/ paramLists.flatten.map(parseArgument(_, paramPrefix)).asJava, // TODO add support for parameters
      /*documentation =*/ methodSymbol.documentation.asJava,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ methodSymbol.source.asJava,
      /*visibility =*/ placeholderVisibility,
      /*type =*/ method.returnTpt.dokkaType,
      /*generics =*/ genericTypes.map(parseTypeArgument).asJava,
      /*receiver =*/ null, // Not used
      /*modifier =*/ placeholderModifier,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
        plus MethodExtension(paramLists.map(_.size))
        plus(MemberExtension(
          methodSymbol.getVisibility(), 
          methodSymbol.getExtraModifiers(), 
          methodKind, 
          methodSymbol.getAnnotations()))
    )

  def parseArgument(argument: ValDef, prefix: Symbol => String, isExtendedSymbol: Boolean = false, isGrouped: Boolean = false): DParameter =
    new DParameter(
      argument.symbol.dri,
      prefix(argument.symbol) + argument.symbol.name,
      argument.symbol.documentation.asJava,
      null,
      argument.tpt.dokkaType,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
        .plus(ParameterExtension(isExtendedSymbol, isGrouped))
        .plus(MemberExtension.empty.copy(annotations = argument.symbol.getAnnotations()))
    )

  def parseTypeArgument(argument: TypeDef): DTypeParameter =
    // Not sure if we should have such hacks...
    val variancePrefix =
      if  argument.symbol.flags.is(Flags.Covariant) then "+"
      else if argument.symbol.flags.is(Flags.Contravariant) then "-"
      else ""

    new DTypeParameter(
      argument.symbol.dri,
      variancePrefix + argument.symbol.name,
      argument.symbol.documentation.asJava,
      null,
      List(argument.rhs.dokkaType).asJava,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
    )

  def parseTypeDef(typeDef: TypeDef): DProperty =

    def isTreeAbstract(typ: Tree): Boolean = typ match {
      case TypeBoundsTree(_, _) => true
      case LambdaTypeTree(params, body) => isTreeAbstract(body)
      case _ => false
    }


    val (generics, tpeTree) = typeDef.rhs match
      case LambdaTypeTree(params, body) => (params.map(parseTypeArgument), body)
      case tpe => (Nil, tpe)

    new DProperty(
      typeDef.symbol.dri,
      typeDef.name,
      /*documentation =*/ typeDef.symbol.documentation.asJava,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ typeDef.symbol.source.asJava,
      /*visibility =*/ placeholderVisibility, 
      /*type =*/ tpeTree.dokkaType, // TODO this may be hard...
      /*receiver =*/ null, // Not used
      /*setter =*/ null,
      /*getter =*/ null,
      /*modifier =*/ placeholderModifier,
      sourceSet.toSet(),
      /*generics =*/ generics.asJava, // TODO
      PropertyContainer.Companion.empty() plus MemberExtension(
        typeDef.symbol.getVisibility(),
        typeDef.symbol.getExtraModifiers(), 
        Kind.Type(!isTreeAbstract(typeDef.rhs), typeDef.symbol.isOpaque),
        typeDef.symbol.getAnnotations()
        )
    )

  def parseValDef(valDef: ValDef): DProperty =
    def givenInstance = Some(valDef.symbol.moduleClass)
        .filter(_.exists)
        .map(_.tree.asInstanceOf[ClassDef])
        .flatMap(_.getParents.headOption)
        .map(_.dokkaType.asSignature)
    
    def defaultKind = if valDef.symbol.flags.is(Flags.Mutable) then Kind.Var else Kind.Val
    val kind = 
      if valDef.symbol.isGiven then Kind.Given(givenInstance, extractImplicitConversion(valDef.tpt.tpe))
      else if valDef.symbol.flags.is(Flags.Implicit) then 
        Kind.Implicit(Kind.Val, extractImplicitConversion(valDef.tpt.tpe))
      else defaultKind

    new DProperty(
      valDef.symbol.dri,
      valDef.name,
      /*documentation =*/ valDef.symbol.documentation.asJava,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ valDef.symbol.source.asJava,
      /*visibility =*/ placeholderVisibility,
      /*type =*/ valDef.tpt.dokkaType,
      /*receiver =*/ null, // Not used
      /*setter =*/ null,
      /*getter =*/ null,
      /*modifier =*/ placeholderModifier,
      sourceSet.toSet(),
      /*generics =*/ Nil.asJava,
      PropertyContainer.Companion.empty().plus(MemberExtension(
          valDef.symbol.getVisibility(), 
          valDef.symbol.getExtraModifiers(), 
          kind,
          valDef.symbol.getAnnotations()
      ))
    )
  
