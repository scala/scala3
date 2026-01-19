package dotty.tools.scaladoc
package tasty
package comments

import scala.quoted._

trait MemberLookup {

  def memberLookupResult(using Quotes)(
    symbol: reflect.Symbol,
    label: String,
    inheritingParent: Option[reflect.Symbol] = None
  ): (reflect.Symbol, String, Option[reflect.Symbol]) =
    (symbol, label, inheritingParent)

  def lookup(using Quotes, DocContext)(
    query: Query,
    owner: reflect.Symbol,
  ): Option[(reflect.Symbol, String, Option[reflect.Symbol])] = lookupOpt(query, Some(owner))

  def lookupOpt(using Quotes, DocContext)(
    query: Query,
    ownerOpt: Option[reflect.Symbol],
  ): Option[(reflect.Symbol, String, Option[reflect.Symbol])] =
    try
      import reflect._

      def nearestClass(sym: Symbol): Symbol =
        if sym.isClassDef then sym else nearestClass(sym.owner)

      def nearestPackage(sym: Symbol): Symbol =
        if sym.flags.is(Flags.Package) then sym else nearestPackage(sym.owner)

      def nearestMember(sym: Symbol): Symbol =
        if sym.isClassDef || sym.flags.is(Flags.Package) then sym else nearestMember(sym.owner)

      val res: Option[(Symbol, String, Option[Symbol])] = {
        def toplevelLookup(querystrings: List[String]) =
          downwardLookup(querystrings, defn.PredefModule.moduleClass)
          .orElse(downwardLookup(querystrings, defn.ScalaPackage))
          .orElse(downwardLookup(querystrings, defn.RootPackage))
          .orElse(downwardLookup(querystrings, defn.EmptyPackageClass))

        ownerOpt match {
          case Some(owner) =>
            val nearest = nearestMember(owner)
            val nearestCls = nearestClass(owner)
            val nearestPkg = nearestPackage(owner)
            def relativeLookup(querystrings: List[String], owner: Symbol): Option[(Symbol, Option[Symbol])] = {
              val isMeaningful =
                owner.exists
                // those are just an optimisation, they can be dropped if problems show up
                && owner.ne(defn.ScalaPackage)
                && owner.ne(defn.RootClass)
                && owner.ne(defn.EmptyPackageClass)

              if !isMeaningful then None else {
                downwardLookup(querystrings, owner) match {
                  case None => relativeLookup(querystrings, owner.owner)
                  case some => some
                }
              }
            }

            query match {
              case Query.StrictMemberId(id) =>
                downwardLookup(List(id), nearest).map(memberLookupResult(_, id, _))
              case Query.QualifiedId(Query.Qual.This, _, rest) =>
                downwardLookup(rest.asList, nearestCls).map(memberLookupResult(_, rest.join, _))
              case Query.QualifiedId(Query.Qual.Package, _, rest) =>
                downwardLookup(rest.asList, nearestPkg).map(memberLookupResult(_, rest.join, _))
              case query =>
                val ql = query.asList
                toplevelLookup(ql)
                .orElse(relativeLookup(ql, nearest))
                .map(memberLookupResult(_, query.join, _))
            }

          case None =>
            toplevelLookup(query.asList).map(memberLookupResult(_, query.join, _))
        }
      }

      // println(s"looked up `$query` in ${owner.show}[${owner.flags.show}] as ${res.map(_.show)}")

      res
    catch
      case e: Exception =>
        if (!summon[DocContext].args.noLinkWarnings) then
          val msg = s"Unable to find a link for ${query} ${ownerOpt.fold("")(o => "in " + o.name)}"
          report.warn(msg, e)
        None

  private def hackMembersOf(using Quotes)(rsym: reflect.Symbol) = {
    import reflect._
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    val members =
      sym.info.allMembers.iterator.map(_.symbol).filter(
        s => hackIsNotAbsent(s.asInstanceOf[Symbol])
      )
    // println(s"members of ${sym.show} : ${members.map(_.show).mkString(", ")}")
    members.asInstanceOf[Iterator[Symbol]]
  }

  private def hackIsNotAbsent(using Quotes)(rsym: reflect.Symbol) =
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    // note: Predef has .info = NoType for some reason
    val iorc = sym.infoOrCompleter
    iorc match
      case _: dotc.core.SymDenotations.ModuleCompleter | dotc.core.SymDenotations.NoCompleter | dotc.core.Types.NoType | _: dotc.core.NoLoader => false
      case _ => true

  private def localLookup(using Quotes)(
    sel: MemberLookup.Selector,
    owner: reflect.Symbol
  ): Iterator[reflect.Symbol] = {
    import reflect._

    def findMatch(syms: Iterator[Symbol]): Iterator[Symbol] = {
      def matches(s: Symbol): Boolean =
        s.name == sel.ident && sel.kind.match {
          case MemberLookup.SelectorKind.ForceTerm => s.isTerm
          case MemberLookup.SelectorKind.ForceType => s.isType
          case MemberLookup.SelectorKind.NoForce => true
        }

      def hackResolveModule(s: Symbol): Symbol =
        if s.flags.is(Flags.Module) then s.moduleClass else s

      // val syms0 = syms.toList
      // val matched0 = syms0.filter(matches)
      // if matched0.isEmpty then
      //   println(s"Failed to look up ${sel.ident} in $owner; all members: {{{")
      //   syms0.foreach { s => println(s"\t$s") }
      //   println("}}}")
      // val matched = matched0.iterator

      // def showMatched() = matched0.foreach { s =>
      //   println(s"\t $s")
      // }
      // println(s"localLookup in class ${owner} for `${sel.ident}`{kind=${sel.kind}}:{{{")
      // showMatched()
      // println("}}}")

      val matched = syms.filter(matches)
      matched.map(hackResolveModule)
    }

    if owner.isPackageDef then
      findMatch(hackMembersOf(owner))
    else
      owner.tree match {
        case tree: TypeDef =>
          val tpe =
            tree.rhs match {
              case tb : TypeBoundsTree => tb.hi.tpe
              case tpt: TypeTree => tpt.tpe
            }

          tpe.classSymbol match {
            case Some(s) => findMatch(hackMembersOf(s))
            case None => Iterator.empty
          }
        case _ =>
          findMatch(hackMembersOf(owner))
      }
  }

  private def downwardLookup(using Quotes)(
    query: List[String], owner: reflect.Symbol
  ): Option[(reflect.Symbol, Option[reflect.Symbol])] = {
    import reflect._
    query match {
      case Nil => None
      case q :: Nil =>
        val sel = MemberLookup.Selector.fromString(q)
        val res = sel.kind match {
          case MemberLookup.SelectorKind.NoForce =>
            // Extract just the method name from the signature (removing type params and param list)
            val methodName = extractMethodName(sel.ident)
            val nameSel = MemberLookup.Selector(methodName, sel.kind)
            val lookedUp = localLookup(nameSel, owner).toSeq
            // note: those flag lookups are necessary b/c for objects we return their classes
            val typeMatch = lookedUp.find(s => s.isType && !s.flags.is(Flags.Module))
            if typeMatch.isDefined then typeMatch
            else {
              val termMatches =
                lookedUp.filter(s => s.isTerm || s.flags.is(Flags.Module))
              if termMatches.size <= 1 then termMatches.headOption
              else {
                // Multiple overloads found, try to match based on signature in query
                parseSignatureParams(sel.ident) match {
                  case Some(queryParamTypes) =>
                    // Signature was provided, try to find matching overload
                    termMatches.find { sym =>
                      matchesParameterTypes(sym, queryParamTypes)
                    }.orElse(termMatches.headOption)
                  case None =>
                    // No signature provided, fall back to first match
                    termMatches.headOption
                }
              }
            }
          case _ =>
            localLookup(sel, owner).nextOption
        }
        res match {
          case None => None
          case Some(sym) =>
            val externalOwner: Option[reflect.Symbol] =
              if owner eq sym.owner then None
              else if owner.flags.is(Flags.Module) && !owner.flags.is(Flags.Package) then Some(owner.moduleClass)
              else if owner.isClassDef then Some(owner)
              else None
            Some(sym -> externalOwner)
        }
      case q :: qs =>
        val sel = MemberLookup.Selector.fromString(q)
        val lookedUp = localLookup(sel, owner).toSeq

        if lookedUp.isEmpty then None else {
          // tm/tp - term/type symbols which we looked up and which allow further lookup
          // pk - package symbol
          // Note: packages collide with both term and type definitions
          // Note: classes and types collide
          var pk: Option[Symbol] = None
          var tp: Option[Symbol] = None
          var tm: Option[Symbol] = None
          lookedUp.foreach { s =>
            if s.isPackageDef then pk = Some(s)
            else if s.flags.is(Flags.Module) then tm = Some(s)
            else if s.isClassDef || s.isTypeDef then tp = Some(s)
          }
          pk.flatMap(downwardLookup(qs, _))
          .orElse(tp.flatMap(downwardLookup(qs, _)))
          .orElse(tm.flatMap(downwardLookup(qs, _)))
        }
    }
  }

  /** Parse parameter types from a method signature.
   *  Input: "[A](b:scala.collection.mutable.Buffer[A],c:Int)"
   *  Output: Some(List("scala.collection.mutable.Buffer", "Int"))
   *
   *  Returns:
   *  - None if no parameter list is present (e.g., "method" or "method[A]")
   *  - Some(Nil) if parameter list is empty (e.g., "method()")
   *  - Some(List(...)) if parameters are present
   */
  private def parseSignatureParams(signature: String): Option[List[String]] = {
    // Normalize the signature by removing backslashes (they're used in scaladoc to escape dots)
    val normalizedSignature = signature.replace("\\.", ".")

    // Find the parameter list (after any type params)
    val parenStart = normalizedSignature.indexOf('(')
    if parenStart == -1 then return None

    val parenEnd = normalizedSignature.lastIndexOf(')')
    if parenEnd == -1 || parenEnd <= parenStart then return None

    val paramList = normalizedSignature.substring(parenStart + 1, parenEnd)
    if paramList.isEmpty then return Some(Nil)

    // Split by comma, respecting nested brackets
    val params = scala.collection.mutable.ListBuffer[String]()
    var depth = 0
    var start = 0
    for i <- 0 until paramList.length do
      val c = paramList.charAt(i)
      if c == '[' then depth += 1
      else if c == ']' then depth -= 1
      else if c == ',' && depth == 0 then
        params += paramList.substring(start, i).trim
        start = i + 1
    params += paramList.substring(start).trim

    // Extract type from each "name:Type" pair, stripping type arguments
    Some(params.toList.flatMap { param =>
      val colonIdx = param.indexOf(':')
      if colonIdx == -1 then None
      else
        val typePart = param.substring(colonIdx + 1).trim
        // Remove type arguments to get the base type
        val bracketIdx = typePart.indexOf('[')
        val baseType = if bracketIdx == -1 then typePart else typePart.substring(0, bracketIdx)
        Some(baseType)
    })
  }

  /** Extract simple type name from a possibly qualified type.
   *  "scala.collection.mutable.Buffer[A]" -> "Buffer"
   *  "Int" -> "Int"
   */
  private def extractSimpleTypeName(qualifiedType: String): String = {
    // Remove type arguments first
    val withoutTypeArgs = qualifiedType.indexOf('[') match {
      case -1 => qualifiedType
      case i => qualifiedType.substring(0, i)
    }
    // Get last segment after '.'
    withoutTypeArgs.split('.').last
  }

  /** Extract just the method name from a signature, removing type parameters and parameter lists.
   *  For example, from "asJava[A](b:scala.collection.mutable.Buffer[A])*" extracts "asJava".
   */
  private def extractMethodName(signature: String): String = {
    // Find the first occurrence of [ or (, and return everything before it
    val bracketIndex = signature.indexOf('[')
    val parenIndex = signature.indexOf('(')
    if bracketIndex == -1 && parenIndex == -1 then signature
    else {
      val firstSpecial = if bracketIndex == -1 then parenIndex else if parenIndex == -1 then bracketIndex else math.min(bracketIndex, parenIndex)
      signature.substring(0, firstSpecial)
    }
  }

  /** Check if a method's parameter types match the expected parameter types from the query.
   *  This compares the simple type names of all parameters in order, which could, theoretically,
   * have edge cases with same-named types in different packages
   */
  private def matchesParameterTypes(using Quotes)(sym: reflect.Symbol, queryParamTypes: List[String]): Boolean = {
    import reflect._

    def getMethodParamTypes(tpe: TypeRepr): Option[List[String]] = tpe match {
      case MethodType(_, paramTypes, _) =>
        Some(paramTypes.map(pt => extractSimpleTypeName(pt.show)))
      case PolyType(_, _, resType) =>
        getMethodParamTypes(resType)
      case _ => None
    }

    try {
      getMethodParamTypes(sym.info) match {
        case Some(actualTypes) =>
          val querySimpleTypes = queryParamTypes.map(extractSimpleTypeName)
          actualTypes == querySimpleTypes
        case None => false
      }
    } catch {
      case _: Exception => false
    }
  }
}

object MemberLookup extends MemberLookup {
  enum SelectorKind {
    case ForceTerm
    case ForceType
    case NoForce
  }

  case class Selector(ident: String, kind: SelectorKind)
  object Selector {
    def fromString(str: String) = {
      // Scaladoc overloading support allows terminal * (and they're meaningless)
      val cleanStr = str.stripSuffix("*")

      if cleanStr.endsWith("$") then
        Selector(cleanStr.init, SelectorKind.ForceTerm)
      else if cleanStr.endsWith("!") then
        Selector(cleanStr.init, SelectorKind.ForceType)
      else
        Selector(cleanStr, SelectorKind.NoForce)
    }
  }
}
