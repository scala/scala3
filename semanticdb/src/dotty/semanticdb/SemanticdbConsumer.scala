package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import dotty.tools.dotc.core.StdNames._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.meta.internal.{semanticdb => s}
import dotty.semanticdb.Scala.{Descriptor => d}
import dotty.semanticdb.Scala._


class SemanticdbConsumer(sourceFilePath: java.nio.file.Path) extends TastyConsumer {
  var stack: List[String] = Nil

  val semantic: s.TextDocument = s.TextDocument()
  var occurrences: Seq[s.SymbolOccurrence] = Seq()

  def toSemanticdb(): s.TextDocument = {
    s.TextDocument(text = sourceCode.content(), occurrences = occurrences)
  }

  // Caching for package definitions (as they are shared accross different class files)
  val packageDefinitions: Set[(String, Int)] = Set()
  // Caching for symbol paths to avoid regenerating some of them
  //  (as computing a symbol path from a symbol is deterministic)
  val symbolsCache: HashMap[(Any, s.Range), String] = HashMap()
  // Offset for local symbol
  var localOffset: Int = 0

  val sourceCode = new SourceFile(sourceFilePath)

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    // To avoid adding symbol paths duplicates inside a same class
    val symbolPathsMap: Set[(String, s.Range)] = Set()

    object ChildTraverser extends TreeTraverser {
      var children: List[Tree] = Nil
      var childrenType: List[Tree /*TypeTree | TypeBoundsTree*/] = Nil
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case IsTypeTree(tree) =>
          traverseTypeTree(tree)
        case IsTypeBoundsTree(tree) =>
          traverseTypeTree(tree)
        case _ => children = tree :: children
      }
      override def traversePattern(pattern: Pattern)(
          implicit ctx: Context): Unit = ()
      def traverseTypeTree(tree: Tree /*TypeTree | TypeBoundsTree*/)(
          implicit ctx: Context): Unit =
          childrenType = tree :: childrenType

      def getChildren(tree: Tree)(implicit ctx: Context): List[Tree] = {
        children = Nil
        traverseTreeChildren(tree)(ctx)
        return children
      }
      def getChildrenType(tree: Tree /*TypeTree | TypeBoundsTree*/)(implicit ctx: Context): List[Tree /*TypeTree | TypeBoundsTree*/] = {
        childrenType = Nil
        traverseTreeChildren(tree)(ctx)
        return childrenType
      }
    }

    /* The (==) operator does not work correctly on two positions,
    we redefine our one */
    def arePositionEqual(p1 : Position, p2 : Position) : Boolean = {
      p1.start == p2.start &&
      p1.end == p2.end &&
      p1.sourceFile == p2.sourceFile
    }

    object Traverser extends TreeTraverser {
      implicit class TreeExtender(tree: Tree) {
        def isUserCreated: Boolean = {
          val children: List[Position] =
            ChildTraverser.getChildren(tree)(reflect.rootContext).map(_.pos)
          return !((tree.pos.exists && tree.pos.start == tree.pos.end && children == Nil) ||
            children.exists(arePositionEqual(tree.pos, _)))
        }
      }

      implicit class TypeTreeExtender(tree: TypeTree) {
        def isUserCreated: Boolean = {
          val children: List[Position] =
            ChildTraverser.getChildrenType(tree)(reflect.rootContext).collect(_ match {
            case IsTypeTree(tt) => tt.pos})
          return !((tree.pos.exists && tree.pos.start == tree.pos.end && children == Nil) ||
            children.exists(arePositionEqual(tree.pos, _)))
        }
      }

      implicit class TypeOrBoundsTreeExtender(tree: Tree /*TypeTree | TypeBoundsTree*/) {
        def typetree: TypeTree = tree match {
          case IsTypeTree(t) => t
        }
      }

      implicit class PatternExtender(tree: Pattern) {
        def isUserCreated: Boolean = {
          return !(tree.pos.exists && tree.pos.start == tree.pos.end)
        }
      }

      implicit class SymbolExtender(symbol: Symbol) {
        def exists = !(symbol.name == "<none>" || symbol == NoSymbol)
        /* Return true if symbol represents the definition of a var setter, false otherwise.
        We return true if the extract of source code corresponding to the position of the symbol is the same as the symbol name.
        Ex:
        var m = ???
        -> there is a defdef for `m_=` with position "m =". As "m =" != "m_=", we return false
        */
        def isMutableSetterExplicit(role : s.SymbolOccurrence.Role) = {
          if (role == s.SymbolOccurrence.Role.DEFINITION &&
              symbol.pos.exists &&
              symbol.flags.is(Flags.Mutable) && symbol.isMethod  &&
              symbol.trueName.endsWith("_="))
            (sourceCode.peek(symbol.pos.start, symbol.pos.end) == symbol.trueName)
          else
            true
        }

        // The name of a symbol can contain special chars. This will replace them with the correct char.
        def trueName: String = {
          val prohibitedChars = '.' :: ';' :: '[' :: '/' :: '<' :: '>' :: Nil
          prohibitedChars.foldLeft(symbol.name)((old, chr) =>
            old.replaceAll("\\$u%04X".format(chr.toInt), chr.toString)
          )
        }


        def isClass: Boolean = symbol match {
          case IsClassDefSymbol(_) => true
          case _                => false
        }

        def isTypeParameter: Boolean = symbol.isParameter && symbol.isType

        def isType: Boolean = symbol match {
          case IsTypeDefSymbol(_) => true
          case _               => false
        }

        def isTerm: Boolean = !symbol.isType

        def isMethod: Boolean = symbol match {
          case IsDefDefSymbol(_) => true
          case _              => false
        }

        def isVal: Boolean = symbol match {
          case IsValDefSymbol(_) => true
          case _              => false
        }

        def isPackage: Boolean = symbol match {
          case IsPackageDefSymbol(_) => true
          case _                  => false
        }

        def isDefaultGetter: Boolean =
          symbol.name.contains(tpnme.DEFAULT_GETTER.toString)

        def isReservedName : Boolean = {
          val keywords =
            List("ev$", "evidence$", "$_lazy_implicit_$", "$lzy", "$lzyINIT",
              "$OFFSET", "bitmap$", "_$", "$tailLocal", "tmp", "$doc",
              "$superArg$", "$scrutinee", "$elem")
          return keywords.exists(symbol.name.contains(_))
        }

        def isParameter: Boolean = symbol.flags.is(Flags.Param)

        def isObject: Boolean = symbol.flags.is(Flags.Object)

        def isTrait: Boolean = symbol.flags.is(Flags.Trait)

        def isConstructor(implicit ctx: Context): Boolean =
          symbol.name == "<init>"

        def isVarAccessor(implicit ctx: Context): Boolean = {
          symbol.isVal && symbol.flags.is(Flags.Mutable)
        }

        def isValMethod(implicit ctx: Context): Boolean = {
          symbol.isMethod && {
            (symbol.flags.is(Flags.FieldAccessor)  && symbol.flags.is(Flags.StableRealizable) ) ||
            (symbol.isUsefulField && !symbol.flags.is(Flags.Mutable) )
          }
        }

        def isAnonymousClassConstructor(implicit ctx: Context): Boolean = {
          symbol.isConstructor && symbol.owner.isAnonymousClass
        }

        def isAnonymousSelfParameter(implicit ctx: Context): Boolean = {
          symbol.isSelfParameter && {
            symbol.name == tpnme.this_.toString || // hardlinked in ClassSignature.self
            symbol.name.startsWith("x$") // wildcards can't be referenced: class A { _: B => }
          }
        }

        def isWildCard(implicit ctx: Context): Boolean = {
          symbol.name.startsWith(tpnme.WILDCARD.toString) &&
          symbol.name != tpnme.THIS.toString
        }

        def isAnonymousInit(implicit ctx: Context): Boolean = {
          return symbol.exists && symbol.owner.exists &&
          (symbol.owner.isAnonymousFunction || symbol.owner.isAnonymousClass) &&
          symbol.name == "<init>"
        }

        /* The following methods are directly extracted from the scala
        implementation of SemanticDB (scalameta/semanticdb/scalac/library/src/main/scala/scala/meta/internal/semanticdb/scalac/SymbolOps.scala)
        */
        def isValueParameter: Boolean = symbol.isParameter && !symbol.isType && !symbol.flags.is(Flags.ParamAccessor)

        def isJavaClass: Boolean = (symbol.isClass || symbol.isObject) && symbol.flags.is(Flags.JavaDefined)

        def isSelfParameter(implicit ctx: Context): Boolean =
          symbol.exists && symbol.owner == symbol

        def isSemanticdbLocal(implicit ctx: Context): Boolean = {
          def definitelyGlobal = symbol.isPackage
          def definitelyLocal =
            !symbol.exists ||
              (symbol.owner.isTerm && !symbol.isParameter) ||
              ((symbol.owner.isAliasType || symbol.owner.isAbstractType) && !symbol.isParameter) ||
              symbol.isSelfParameter ||
              symbol.isLocalDummy ||
              symbol.isRefinementClass ||
              symbol.isAnonymousClass ||
              symbol.isAnonymousFunction /*||
              symbol.isExistential*/
          def ownerLocal = symbol.owner.isSemanticdbLocal
          !definitelyGlobal && (definitelyLocal || ownerLocal)
        }

        def isSyntheticConstructor(implicit ctx: Context): Boolean = {
          val isObjectConstructor = symbol.isConstructor && symbol.owner.exists && symbol.owner.flags.is(Flags.Object)
          val isModuleConstructor = symbol.isConstructor && symbol.owner.isClass
          val isTraitConstructor = symbol.isConstructor && symbol.owner.isTrait
          val isInterfaceConstructor = symbol.isConstructor && symbol.owner.flags.is(Flags.JavaDefined)  && symbol.owner.isTrait
          val isEnumConstructor = symbol.isConstructor && symbol.owner.flags.is(Flags.JavaDefined) && symbol.owner.flags.is(Flags.Enum)
          /*val isStaticConstructor = symbol.name == g.TermName("<clinit>")*/
          //val isClassfileAnnotationConstructor = symbol.owner.isClassfileAnnotation
          /*isModuleConstructor || */
          isTraitConstructor || isInterfaceConstructor || isObjectConstructor ||
          isEnumConstructor /*|| isStaticConstructor || isClassfileAnnotationConstructor*/
        }
        def isLocalChild(implicit ctx: Context): Boolean =
          symbol.name == tpnme.LOCAL_CHILD.toString

        def isSyntheticValueClassCompanion(implicit ctx: Context): Boolean = {
          if (symbol.isClass) {
            if (symbol.flags.is(Flags.Object)) {
              symbol.asClassDef.moduleClass.fold(false)(c =>
                c.isSyntheticValueClassCompanion)
            } else {
              symbol.flags.is(Flags.ModuleClass) &&
              symbol.flags.is(Flags.Synthetic) &&
              symbol.asClassDef.methods.isEmpty
            }
          } else {
            false
          }
        }

        /* the `isFieldForPrivateThis` is commented out otherwise class members of the form
          "private[this] val foo" are not converted to symbol occurences.
          In the original semanticdb this line is commented out.
        */
        def isScalacField(implicit ctx: Context): Boolean = {
          //val isFieldForPrivateThis = symbol.flags.is(Flags.PrivateLocal)  && symbol.isTerm && !symbol.isMethod && !symbol.isObject
          val isFieldForOther = false //symbol.name.endsWith(g.nme.LOCAL_SUFFIX_STRING)
          val isJavaDefined = symbol.flags.is(Flags.JavaDefined)
          (/*isFieldForPrivateThis ||*/ isFieldForOther) && !isJavaDefined
        }
        def isUselessField(implicit ctx: Context): Boolean = {
          symbol.isScalacField && symbol.owner.exists
        }
        def isUsefulField(implicit ctx: Context): Boolean = {
          symbol.isScalacField && !symbol.isUselessField
        }
        def isSyntheticCaseAccessor(implicit ctx: Context): Boolean = {
          symbol.flags.is(Flags.CaseAcessor) && symbol.trueName.contains("$")
        }
        def isSyntheticJavaModule(implicit ctx: Context): Boolean = {
          val resolved = symbol match {
          case IsClassDefSymbol(c) => resolveClass(c)
          case _ => symbol
          }
          !resolved.flags.is(Flags.Package)  && resolved.flags.is(Flags.JavaDefined)  && resolved.flags.is(Flags.Object)
        }
        def isSyntheticAbstractType(implicit ctx: Context): Boolean = {
          symbol.flags.is(Flags.Synthetic)  && symbol.isAbstractType // these are hardlinked to TypeOps
        }
        def isEtaExpandedParameter(implicit ctx: Context): Boolean = {
          // Term.Placeholder occurrences are not persisted so we don't persist their symbol information.
          // We might want to revisit this decision https://github.com/scalameta/scalameta/issues/1657
          symbol.isParameter &&
          symbol.name.startsWith("x$") &&
          symbol.owner.isAnonymousFunction
        }
        def isStaticMember(implicit ctx: Context): Boolean =
          symbol.exists &&
            (symbol.flags.is(Flags.Static)  ||
              /*symbol.annots.find(_ == ctx.definitions.ScalaStaticAnnot)*/ false)

        def isStaticConstructor(implicit ctx: Context): Boolean = {
          (symbol.isStaticMember && symbol.isClassConstructor) || (symbol.name == tpnme.STATIC_CONSTRUCTOR.toString)
        }

        /* End of methods imported from the scala version of SemanticDB */

        def isInitChild(implicit ctx: Context): Boolean = {
          if (symbol.exists && symbol.owner.exists) {
            return symbol.owner.name == "<init>" || symbol.owner.isInitChild
          } else {
            return false
          }
        }

        def isUseless(implicit ctx: Context): Boolean = {
          !symbol.exists ||
          symbol.isReservedName ||
          symbol.isAnonymousInit ||
          symbol.isDefaultGetter ||
          symbol.isWildCard ||
          symbol.isAnonymousClass ||
          symbol.isAnonymousFunction ||
          symbol.isSyntheticConstructor ||
          symbol.isStaticConstructor ||
          symbol.isLocalChild ||
          symbol.isSyntheticValueClassCompanion ||
          symbol.isUselessField ||
          symbol.isSyntheticCaseAccessor ||
          symbol.isRefinementClass ||
          symbol.isSyntheticJavaModule
        }
        def isUseful(implicit ctx: Context): Boolean = !symbol.isUseless
        def isUselessOccurrence(implicit ctx: Context): Boolean = {
          symbol.isUseless &&
          !symbol.isSyntheticJavaModule // references to static Java inner classes should have occurrences
        }
      }

      def resolveClass(symbol: ClassDefSymbol): Symbol =
        (symbol.companionClass, symbol.companionModule) match {
          case (Some(c), _)                               => c
          case (_, Some(module)) if symbol.flags.is(Flags.Object) => module
          case _                                          => symbol
        }

      def disimbiguate(symbolPath: String, symbol: Symbol): String = {
        try {
          val symbolcl = resolveClass(symbol.owner.asClassDef)
          symbolcl match {
            case IsClassDefSymbol(classsymbol) => {
              val methods = classsymbol.method(symbol.name)
              val (methods_count, method_pos) =
                methods.foldLeft((0, -1))((x: Tuple2[Int, Int], m: Symbol) => {
                  if (m == symbol)
                    (x._1 + 1, x._1)
                  else
                    (x._1 + 1, x._2)
                })
              val real_pos = methods_count - method_pos - 1

              if (real_pos == 0) {
                "()"
              } else {
                "(+" + real_pos + ")"
              }
            }
            case _ => {
              "()"
            }
          }
        } catch {
          case _ => "()"
        }
      }

      def iterateParent(symbol: Symbol, isMutableAssignement:Boolean=false): String = {
        if (!symbol.exists || symbol.name == "<root>") then {
          ""
        } else {
          val rsymbol = symbol match {
            case IsClassDefSymbol(c) => resolveClass(c)
            case _ => symbol
          }
          val previous_symbol =
            /* When we consider snipper of the form: `abstract class DepAdvD[CC[X[C] <: B], X[Z], C] extends DepTemp`,
              The symbol for C will be something like example/DepAdvD#`<init>`().[CC].[X].[C].
              This is illogic: a init method can't have any child. Thus, when the current symbol is
              a typeparameter, and the owner is an init, we can just "jump" over the init. */
            if (rsymbol.owner.name == "<init>" && rsymbol.isType)
              iterateParent(rsymbol.owner.owner)
            else
              iterateParent(rsymbol.owner)


          val isdef = rsymbol match {case IsDefDefSymbol(_) => true case _ => false}
          val symbolName = if (isMutableAssignement) rsymbol.trueName + "_=" else rsymbol.trueName
          val next_atom =
            if (rsymbol.isPackage) {
              d.Package(symbolName)
            } else if (rsymbol.isObject && !rsymbol.isJavaClass) {
              d.Term(symbolName)
            } else if (rsymbol.isValMethod && !rsymbol.isVarAccessor) {
              d.Term(symbolName)
            } else if (rsymbol.isMethod || rsymbol.isUsefulField || rsymbol.isVarAccessor) {
              d.Method(symbolName,
                       disimbiguate(previous_symbol + symbolName, rsymbol))
            } else if (rsymbol.isTypeParameter) {
              d.TypeParameter(symbolName)
            } else if (rsymbol.isValueParameter) {
              d.Parameter(symbolName)
            } else if (rsymbol.isType || rsymbol.isJavaClass) {
              d.Type(symbolName)
            } else {
              d.Term(symbolName)
            }

          Symbols.Global(previous_symbol, next_atom)
        }
      }

      def symbolToSymbolString(symbol: Symbol, isMutableAssignement:Boolean = false): (String, Boolean) = {
        if (symbol.isSemanticdbLocal) {
          var localsymbol = Symbols.Local(localOffset.toString)
          localOffset += 1
          (localsymbol, false)
        } else {
          (iterateParent(symbol, isMutableAssignement), true)
        }
      }

      def addOccurence(symbol: Symbol,
                       typeSymbol: s.SymbolOccurrence.Role,
                       range: s.Range,
                       isMutableAssignement:Boolean = false): Unit = {
        if (!symbol.exists) return

        val symbolName = if (isMutableAssignement) symbol.trueName + "_=" else symbol.trueName
        val (symbolPath, isGlobal) =
          if (symbol.pos.exists) {
            val keyRange = createRange(symbol.pos)
            if (symbolsCache.contains((symbolName, keyRange)))
              (symbolsCache((symbolName, keyRange)), symbol.isSemanticdbLocal)
            else {
              val (sp, ig) = symbolToSymbolString(symbol, isMutableAssignement)
              symbolsCache += ((symbolName, keyRange) -> sp)
              (sp, ig)
            }
          } else {
            symbolToSymbolString(symbol)
          }

        if (symbolPath == "") return
        if (symbol.flags.is(Flags.Synthetic) && typeSymbol == s.SymbolOccurrence.Role.DEFINITION) return

        val key = (symbolPath, range)        // this is to avoid duplicates symbols
        // For example, when we define a class as: `class foo(x: Int)`,
        // dotty will generate a ValDef for the x, but the x will also
        // be present in the constructor, thus making a double definition
        if (!symbolPathsMap.contains(key)) {
          symbolPathsMap += key
          occurrences =
            occurrences :+
              s.SymbolOccurrence(
                Some(range),
                symbolPath,
                typeSymbol
              )
        }
      }

      def addOccurencePredef(parent: String, name: String, range: s.Range): Unit = {
        occurrences =
          occurrences :+
            s.SymbolOccurrence(
              Some(range),
              parent + name + "().",
              s.SymbolOccurrence.Role.DEFINITION
            )
      }

      def addSelfDefinition(name: String, range: s.Range): Unit = {
        var localsymbol = Symbols.Local(localOffset.toString)
        localOffset += 1
        symbolsCache += ((name, range) -> localsymbol)
        occurrences =
          occurrences :+
            s.SymbolOccurrence(
              Some(range),
              localsymbol,
              s.SymbolOccurrence.Role.DEFINITION
            )
      }

      def addOccurenceTree(tree: Tree,
                           typeSymbol: s.SymbolOccurrence.Role,
                           range: s.Range,
                           forceAdd: Boolean = false,
                           isMutableAssignement: Boolean = false): Unit = {
        if (!tree.symbol.isUselessOccurrence &&
          tree.symbol.isMutableSetterExplicit(typeSymbol) &&
          (tree.isUserCreated || forceAdd)) {
          addOccurence(tree.symbol, typeSymbol, range, isMutableAssignement)
        }
      }

      def addOccurenceTypeTree(typetree: TypeTree,
                               typeSymbol: s.SymbolOccurrence.Role,
                               range: s.Range): Unit = {
        if (!typetree.symbol.isUselessOccurrence && typetree.isUserCreated) {
          addOccurence(typetree.symbol, typeSymbol, range)
        }
      }

      def addOccurencePatternTree(tree: Pattern,
                                  typeSymbol: s.SymbolOccurrence.Role,
                                  range: s.Range): Unit = {
        if (!tree.symbol.isUselessOccurrence && tree.isUserCreated) {
          addOccurence(tree.symbol, typeSymbol, range)
        }
      }

      def addOccurenceId(parentPath: String, id: Id): Unit = {
        val symbolPath = Symbols.Global(parentPath, d.Term(id.name))
        occurrences =
          occurrences :+
            s.SymbolOccurrence(
              Some(createRange(id.pos)),
              symbolPath,
              s.SymbolOccurrence.Role.REFERENCE
            )
      }

      def createRange(pos: Position): s.Range =
      createRange(pos.startLine, pos.startColumn, pos.endLine, pos.endColumn)

      def createRange(startLine : Int, startColumn : Int, length : Int) : s.Range = {
        createRange(startLine, startColumn, startLine, startColumn + length)
      }

      def createRange(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): s.Range = {
        /* This aux function is to make sure every generated range are coherent,
        meaning they all have a valid startLine and startColumn (meaning startColumn is
        a number of byte from the start of the line, not the start of the file)*/
        def aux(l : Int, c : Int) : (Int, Int) = {
          if (l == 0) {
            val line = sourceCode.offsetToLine(l)
            (line, c - sourceCode.lineToOffset(line))
          } else {
            (l, c)
          }
        }

        val (sl, sc) = aux(startLine, startColumn)
        val (el, ec) = aux(endLine, endColumn)
        s.Range(sl, sc, el, ec)
      }

      /* Create a "point range" (a range refering to a point position) */
      def createRange(line: Int, column: Int) : s.Range = {
        createRange(line, column, line, column)
      }

      def rangeSelect(name: String, range: Position): s.Range = {
        if (name == "<init>") {
          return createRange(range)
        } else
        /* The position of a select is the position of the whole select expression,
        from the start to the end.
        To get the position of only the selected operand, we distinguish two cases:
        - either we are selecting an operator ending with a ':' (for those the execution
          order is reversed), so the selected expression is at the start.
          Ex: A #:: B -> the position of the select is the range "#:: B", so we pick the range "#::"
        - either the select is in normal order, in this case we select the end of it.
          Ex: A + B -> the position of the select is the range "A +", so we pick the range "+"
        */
        if (name.endsWith(":")) {
          return createRange(range.startLine, range.startColumn, name.length)
        } else {
          return createRange(range.endLine, range.endColumn - name.length, name.length)
        }
      }

      def getImportPath(pathTerm: Term): String = {
        val range = pathTerm match {
          case Select(qualifier, selected) => {
            getImportPath(qualifier)
            rangeSelect(selected, pathTerm.pos)
          }
          case Ident(x) => {
            createRange(pathTerm.pos.startLine, pathTerm.pos.startColumn, pathTerm.symbol.trueName.length)
          }
        }
        addOccurenceTree(pathTerm,
          s.SymbolOccurrence.Role.REFERENCE,
          range)
        iterateParent(pathTerm.symbol)
      }


      /* A known bug with import path is that we are not able to determine the nature of the
        imported symbol (or to append several of them if we are importing both a class
        and its companionmodule for exemple) */
      def getImportSelectors(parentPath: String,
                             selectors: List[ImportSelector]): Unit = {
        selectors.foreach(selector =>
          selector match {
            case SimpleSelector(id) if id.name != "_" => {
              addOccurenceId(parentPath, id)
            }
            case RenameSelector(id, _) if id.name != "_" => {
              addOccurenceId(parentPath, id)
            }
            case OmitSelector(id) if id.name != "_" => {
              addOccurenceId(parentPath, id)
            }
            case _ =>
        })
      }

      def traverseTypeTree(tree: Tree /*TypeTree | TypeBoundsTree*/)(
          implicit ctx: Context): Unit = {
        tree match {
          case TypeIdent(_) => {
            val typetree = tree.typetree
            addOccurenceTypeTree(typetree,
                                 s.SymbolOccurrence.Role.REFERENCE,
                                 createRange(typetree.pos))
          }
          case TypeSelect(qualifier, _) => {
            val typetree = tree.typetree
            val range = rangeSelect(typetree.symbol.trueName, typetree.pos)
            addOccurenceTypeTree(typetree,
                                 s.SymbolOccurrence.Role.REFERENCE,
                                 range)
            super.traverseTree(typetree)
          }

          case Projection(qualifier, x) => {
              val typetree = tree.typetree
              val range = rangeSelect(typetree.symbol.trueName, typetree.pos)
              addOccurenceTypeTree(typetree,
                                  s.SymbolOccurrence.Role.REFERENCE,
                                  range)
              super.traverseTree(typetree)
          }

          case Inferred() => {
            /* In theory no inferred types should be put in the semanticdb file.
            However, take the case where a typed is refered from an imported class:
              class PrefC {
                object N {
                  type U
                }
              }

              object PrefTest {
                val c: PrefC = ???
                import c.N._
                def k3: U = ???
              }

            The type corresponding to U in the definition of k3 is marked as
            inferred even though it is present in the source code. We use a
            workaround for this specific case, by checking if the name of the
            inferred type corresponds to the one put in the source code at this
            position
            */

            val typetree = tree.typetree
            val start = typetree.pos.start
            val end = typetree.pos.end
            if (sourceCode.peek(start, end) == typetree.symbol.name) {
              addOccurenceTypeTree(typetree,
                                 s.SymbolOccurrence.Role.REFERENCE,
                                 createRange(typetree.pos))
            }
          }

          case _ => {
            super.traverseTree(tree)
          }
        }
      }

      override def traversePattern(tree: Pattern)(implicit ctx: Context): Unit = {
        tree match {
          case Pattern.Bind(name, _) => {
            addOccurencePatternTree(
              tree,
              s.SymbolOccurrence.Role.REFERENCE,
              createRange(tree.symbol.pos.startLine, tree.symbol.pos.startColumn, name.length)
            )
            super.traversePattern(tree)
          }
          case _ =>
            super.traversePattern(tree)
        }
      }


      /* Finding the range of init symbols is not intuitive. We can determine it on a classdef.
      [fittedInitClassRange] is used to transmit this information to the corresponding <init> symbol */
      var fittedInitClassRange: Option[s.Range] = None

      /* At each point of the traversal [classStacks] is the list of classes currently being defined
        Ex:
        class Foo {
          class Bar {
            ??? // classStacks = Bar :: Foo :: Nil
          }
          ??? // classStacks = Foo :: Nil
        }
      */
      var classStacks : List[Symbol] = Nil

      /* Is the term we are currently seeing the rhs of an assignement? */
      var isAssignedTerm = false

      /* Create a mapping from parameter name to parameter position */
      def generateParamsPosMapping(cdef: DefDef)(implicit ctx: Context): Map[String, s.Range] = {
        val DefDef(_, _, params, _, _) = cdef
        val start = Map[String, s.Range]()
        params.foldLeft(start)((old, statements) => {
          statements.foldLeft(old)((old, cval) => {
            old + (cval.name -> createRange(cval.symbol.pos.startLine, cval.symbol.pos.startColumn, cval.symbol.trueName.length))
          })
        }
        )
      }

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case Import(_, path, selectors) =>
            val key = (tree.symbol.trueName, tree.pos.start)
            if (!packageDefinitions(key)) {
              packageDefinitions += key
              getImportSelectors(getImportPath(path), selectors)
            }
          case New(ty) => {
            super.traverseTree(tree)
          }
          case Apply(_, _) => {
            super.traverseTree(tree)
          }
          case ClassDef(classname, constr, parents, derived, selfopt, statements) => {
          val offsetSymbolClass =
              if(tree.symbol.flags.is(Flags.Object)) -1
              else 0

            // we first add the class to the symbol list
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.DEFINITION,
                             createRange(tree.symbol.pos.startLine,
                                         tree.symbol.pos.startColumn + offsetSymbolClass,
                                         tree.symbol.trueName.length))

            /* Before adding the constructor symbol, we must find its position. Two options here:
            - we've got no type parameters: `class Foo {...}` -> the <init> symbol is put after `Foo`
            - we've got some typeparameters: `class Foo[X] {...}` -> the <init> symbol is put after [X]
            In order to find the correct position in the last case, we put ourself on the rightmost bound
            of all type parameters, that means before the last ']'. Then, we move one character right to
            pass the ']' while making sure to skip whitespaces and comments */

            /* The position is put in [fittedInitClassRange] to be transmitted to the defdef of <init> */
            val DefDef(_, typesParameters, _, _, _) = constr
            if (typesParameters.isEmpty) {
              fittedInitClassRange = Some(
                createRange(tree.symbol.pos.startLine,
                        tree.symbol.pos.startColumn + classname.length))
            } else {
              val rightmost = typesParameters.reverse.head.pos.end
              val end_ = sourceCode.nextCharacterSkipComments(rightmost) + 1
              fittedInitClassRange = Some(createRange(0, end_))
            }

            traverseTree(constr)

            fittedInitClassRange = None

            // we add the parents to the symbol list
            parents.foreach(traverseTree)

            selfopt match {
              case Some(vdef @ ValDef(name, type_, _)) => {
                // If name is "_" then it means it is in fact "this". We don't
                // want to had a symbol for it in semanticdb
                if (name != "_") {
                  // The tree does not include a position to the overloaded version of
                  // this. We find it heuristically by "parsing" the source code.
                  // The process is done in three steps:
                  // 1) Find a position before the '{' of the self but after any
                  //  non related '{'. Here, it will be the largest end pos of a parent
                  // 2) Find the first '{'
                  // 3) Iterate until the character we are seeing is a letter
                  val startPosSearch: Int = parents.foldLeft(tree.pos.end)(
                    (old: Int, ct: Tree) =>
                      ct match {
                        case IsTerm(t) if t.pos.end < old => t.pos.end
                        case _                                  => old
                    })

                  var posColumn = if (startPosSearch == tree.pos.end) tree.pos.start else startPosSearch
                  posColumn = sourceCode.firstOccurrenceLetter('{', posColumn)
                  posColumn = sourceCode.nextCharacterSkipComments(posColumn+1)

                  addSelfDefinition(name, createRange(0, posColumn, name.length))
                }
                traverseTypeTree(type_)
              }
              case _ =>
            }

            derived.foreach(traverseTypeTree)

            /* The last part is to go through every statements.
            As usual, we must take care of how we do it as some statements are
            accessors for parameters and we don't want to add duplicate information.
            If a statement is a parameter accessor we add the corresponding occurence as it
            wasn't done when we saw the <init> symbol.
            If it's only a parameter (meaning a type parameter) we already added it
            before, so we do nothing.
            Otherwise we proceed a usual
            */
            classStacks = tree.symbol :: classStacks

            val paramsPosMapping = generateParamsPosMapping(constr)

            statements.foreach(statement => {
              if (statement.symbol.flags.is(Flags.ParamAccessor)) {
                if (paramsPosMapping.contains(statement.symbol.name)) {
                  addOccurenceTree(statement, s.SymbolOccurrence.Role.DEFINITION, paramsPosMapping(statement.symbol.name))
                }
              } else if (!statement.symbol.flags.is(Flags.Param)) {
                traverseTree(statement)
              }
            })

            classStacks = classStacks.tail
          }

          // If we have a <init> symbol with a [fittedInitClassRange] we are sure it is a primary constructor
          // We only record symbols correponding to types as symbols for value parameters will be added
          // by traversing the class statements.
          // Statement should be <EmptyTree> in this case
          case DefDef("<init>", typeparams, valparams, type_, statements) if fittedInitClassRange != None => {
            addOccurenceTree(tree,
                                 s.SymbolOccurrence.Role.DEFINITION,
                                 fittedInitClassRange.get,
                                 true)
            valparams.foreach(params =>
              params.foreach(param => {
                val ValDef(_, tpt, _) = param
                traverseTypeTree(tpt)
              })
            )
            traverseTypeTree(type_)
            typeparams.foreach(traverseTree)
          }

          // An object should have no init symbols
          case DefDef("<init>", _, _, _, _) if tree.symbol.owner.flags.is(Flags.Object) => {
          }

          case Assign(lhs, rhs) => {
            // We make sure to set [isAssignedTerm] to true on the lhs
            isAssignedTerm = true
            traverseTree(lhs)
            isAssignedTerm = false
            traverseTree(rhs)
          }

          case IsDefinition(cdef) => {
            // For a definition we must deal the special case of protected and private
            // definitions
            if (cdef.symbol.flags.is(Flags.Protected)) {
              cdef.symbol.protectedWithin match {
                case Some(within) => {
                  val startColumn = cdef.pos.startColumn + "protected[".length
                  addOccurence(
                    within.typeSymbol,
                    s.SymbolOccurrence.Role.REFERENCE,
                    createRange(cdef.pos.startLine, startColumn, within.typeSymbol.trueName.length)
                  )
                }
                case _ =>
              }
            } else {
              cdef.symbol.privateWithin match {
                case Some(within) => {
                  val startColumn = cdef.pos.startColumn + "private[".length
                  addOccurence(
                    within.typeSymbol,
                    s.SymbolOccurrence.Role.REFERENCE,
                    createRange(cdef.pos.startLine, startColumn, within.typeSymbol.trueName.length)
                  )
                }
                case _ =>
              }
            }

            if (tree.symbol.exists) {
              val pos = tree.symbol.pos
              var rangeSymbol = createRange(pos.startLine, pos.startColumn, tree.symbol.trueName.length)

              // In dotty definition of auxilliary constructors (ex def this(xxxx)) are represented
              // by a DefDef("<init>", ..). This conditions finds such patterns and set a correct rangeSymbol for them.
              if (tree.symbol.trueName == "<init>" && sourceCode.peek(pos.start, pos.start + 4) == "this") {
                rangeSymbol = createRange(pos.startLine, pos.startColumn, 4)
              }
              addOccurenceTree(tree,
                               s.SymbolOccurrence.Role.DEFINITION,
                               rangeSymbol)

            }
            super.traverseTree(cdef)
          }

          case This(Some(id)) => {
            /* We've got two options here:
            - either the this is explicit: eg C.this.XXX. In this case, the position is [C.this], but
              we want to put the symbol on the C, so around id
            - either it is not explicit (eg a.foo). We want to put the symbol only around the a.
            Distinguishing between the two is easy. If the sourcecode between [pos.start; pos.end] ends
            with a 'this', then we're in the first case, otherwise the second
            */
            var rangeThis = createRange(tree.pos)
            if (sourceCode.peek(tree.pos.start, tree.pos.end).endsWith("this")) {
              rangeThis = createRange(tree.pos.startLine, tree.pos.startColumn, tree.symbol.trueName.length)
            }
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.REFERENCE,
                             rangeThis)
          }

          case Super(_, Some(id)) => {
            addOccurence(classStacks.head,
              s.SymbolOccurrence.Role.DEFINITION,
              createRange(id.pos))
            super.traverseTree(tree)
          }

          case Select(qualifier, _) => {
            var range = rangeSelect(tree.symbol.trueName, tree.pos)

            /* This branch deals with select of a `this`. Their is two options:
            - The select of this is explicit (`C.this`). To know if we are in this case we
              check if the end of our position in the sourceCode corresponds to a "this".
            - The select is implicit and was compiler generated. We will force to add it if and only if
              the qualifier itself was user created
            */
            var shouldForceAdd = false
            if (tree.symbol.trueName == "<init>") {
              if (tree.pos.start == tree.pos.end && sourceCode.peek(tree.pos.start - 5, tree.pos.start - 1) == "this") {
                range = createRange(0, tree.pos.start - 5, 4)
                shouldForceAdd = true
              } else {
                range = createRange(tree.pos.endLine, tree.pos.endColumn)
                shouldForceAdd = qualifier.isUserCreated
              }
            }
            /* We do not forget to disable the [isAssignedTerm] flag when exploring the qualifier
            of our select*/
            val temp = isAssignedTerm
            isAssignedTerm = false
            super.traverseTree(tree)
            isAssignedTerm = temp

            /* If we selected a term x which is a mutable variable without a private local flag we want
            to record a call to the function x_= instead. We set the corresponding flag on*/
            val isMutableAssignement = isAssignedTerm && tree.symbol.flags.is(Flags.Mutable) && !tree.symbol.flags.is(Flags.PrivateLocal)
            addOccurenceTree(tree, s.SymbolOccurrence.Role.REFERENCE, range, shouldForceAdd, isMutableAssignement)
          }

          case Ident(name) => {
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.REFERENCE,
                             createRange(tree.pos.startLine, tree.pos.startColumn, tree.symbol.trueName.length))

            super.traverseTree(tree)
          }

          case Inlined(Some(c), b, d) => {
            /* In theory files should be compiled with -Yno-inline before running semanticdb.
            If this is not the case, here is a fallback to heuristically determine which predefFunction
            corresponds to an inlined term.

            We peek the character below the inline node.
            If it is an "l" (for locally), then we have a locally predef call
            If it is an "i" (for implicitly") then it is an implicitly call
            If it is an "a" it is an assert
            */
            def getPredefFunction(pos: Int): String = {
              sourceCode.peek(pos, pos+1) match {
                case "l" => "locally"
                case "i" => "implicitly"
                case "a" => "assert"
              }
            }
            val parentSymbol = iterateParent(c.symbol)
            if (parentSymbol == "dotty/DottyPredef.") {
              val pos = c.pos
              val function = getPredefFunction(pos.start)
              val range = createRange(pos.startLine, pos.startColumn, pos.startLine, function.length)
              addOccurencePredef(parentSymbol, function, range)
            }

            super.traverseTree(tree)
          }

          case PackageClause(_) =>
            val key = (tree.symbol.trueName, tree.pos.start)
            if (!packageDefinitions(key)) {
              addOccurenceTree(tree,
                               s.SymbolOccurrence.Role.DEFINITION,
                               createRange(tree.pos.startLine, tree.pos.startColumn + "package ".length, tree.symbol.trueName.length))
              packageDefinitions += key
            }
            super.traverseTree(tree)

          case tree =>
            super.traverseTree(tree)
        }
      }

    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }

  def println(x: Any): Unit = Predef.println(x)

}
