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
  val package_definitions: Set[Tuple2[String, Int]] = Set()
  val symbolsCache: HashMap[(Any, s.Range), String] = HashMap()
  var local_offset: Int = 0

  val sourceCode = new SourceFile(sourceFilePath)

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    val symbolPathsMap: Set[(String, s.Range)] = Set()

    object ChildTraverser extends TreeTraverser {
      var children: List[Tree] = Nil
      var childrenType: List[TypeOrBoundsTree] = Nil
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit =
        children = tree :: children
      override def traversePattern(pattern: Pattern)(
          implicit ctx: Context): Unit = ()
      override def traverseTypeTree(tree: TypeOrBoundsTree)(
          implicit ctx: Context): Unit =
          childrenType = tree :: childrenType
      override def traverseCaseDef(tree: CaseDef)(implicit ctx: Context): Unit =
        ()
      override def traverseTypeCaseDef(tree: TypeCaseDef)(
          implicit ctx: Context): Unit =
        ()

      def getChildren(tree: Tree)(implicit ctx: Context): List[Tree] = {
        children = Nil
        traverseTreeChildren(tree)(ctx)
        return children
      }
      def getChildrenType(tree: TypeOrBoundsTree)(implicit ctx: Context): List[TypeOrBoundsTree] = {
        childrenType = Nil
        traverseTypeTreeChildren(tree)(ctx)
        return childrenType
      }
    }

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

      implicit class PatternExtender(tree: Pattern) {
        def isUserCreated: Boolean = {
          return !(tree.pos.exists && tree.pos.start == tree.pos.end)
        }
      }

      implicit class SymbolExtender(symbol: Symbol) {
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


        def trueName: String = {
        val prohibitedChars = '.' :: ';' :: '[' :: '/' :: '<' :: '>' :: Nil
              //val prohibitedHashMap = prohibitedChars.map(x => x -> "$u%04X".format(x.toInt)).toMap
              prohibitedChars.foldLeft(symbol.name)((old, chr) =>
                old.replaceAll("\\$u%04X".format(chr.toInt), chr.toString)
              )
        }


        def isClass: Boolean = symbol match {
          case IsClassSymbol(_) => true
          case _                => false
        }

        def isTypeParameter: Boolean = symbol.isParameter && symbol.isType

        def isType: Boolean = symbol match {
          case IsTypeSymbol(_) => true
          case _               => false
        }

        def isTerm: Boolean = !symbol.isType

        def isMethod: Boolean = symbol match {
          case IsDefSymbol(_) => true
          case _              => false
        }

        def isVal: Boolean = symbol match {
          case IsValSymbol(_) => true
          case _              => false
        }

        def isPackage: Boolean = symbol match {
          case IsPackageSymbol(_) => true
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

        def isValueParameter: Boolean = symbol.isParameter && !symbol.isType && !symbol.flags.is(Flags.ParamAccessor)

        def isJavaClass: Boolean = symbol.isClass && symbol.flags.is(Flags.JavaDefined)

        def isSelfParameter(implicit ctx: Context): Boolean =
          symbol != NoSymbol && symbol.owner == symbol

        def isSemanticdbLocal(implicit ctx: Context): Boolean = {
          def definitelyGlobal = symbol.isPackage
          def definitelyLocal =
            symbol == NoSymbol ||
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

        def isConstructor(implicit ctx: Context): Boolean =
          symbol.name == "<init>"

        def isSyntheticConstructor(implicit ctx: Context): Boolean = {
          val isObjectConstructor = symbol.isConstructor && symbol.owner != NoSymbol && symbol.owner.flags.is(Flags.Object)
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
              symbol.asClass.moduleClass.fold(false)(c =>
                c.isSyntheticValueClassCompanion)
            } else {
              symbol.flags.is(Flags.ModuleClass) &&
              symbol.flags.is(Flags.Synthetic) &&
              symbol.asClass.methods.isEmpty
            }
          } else {
            false
          }
        }

        def isVarAccessor(implicit ctx: Context): Boolean = {
          symbol.isVal && symbol.flags.is(Flags.Mutable)
        }

        def isValMethod(implicit ctx: Context): Boolean = {
          symbol.isMethod && {
            (symbol.flags.is(Flags.FieldAccessor)  && symbol.flags.is(Flags.Stable) ) ||
            (symbol.isUsefulField && !symbol.flags.is(Flags.Mutable) )
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
          symbol.isScalacField && symbol.owner != NoSymbol
        }
        def isUsefulField(implicit ctx: Context): Boolean = {
          symbol.isScalacField && !symbol.isUselessField
        }
        def isSyntheticCaseAccessor(implicit ctx: Context): Boolean = {
          symbol.flags.is(Flags.CaseAcessor) && symbol.trueName.contains("$")
        }
        def isSyntheticJavaModule(implicit ctx: Context): Boolean = {
          !symbol.flags.is(Flags.Package)  && symbol.flags.is(Flags.JavaDefined)  && symbol.flags.is(Flags.Object)
        }
        def isAnonymousClassConstructor(implicit ctx: Context): Boolean = {
          symbol.isConstructor && symbol.owner.isAnonymousClass
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
        def isAnonymousSelfParameter(implicit ctx: Context): Boolean = {
          symbol.isSelfParameter && {
            symbol.name == tpnme.this_.toString || // hardlinked in ClassSignature.self
            symbol.name.startsWith("x$") // wildcards can't be referenced: class A { _: B => }
          }
        }
        def isStaticMember(implicit ctx: Context): Boolean =
          (symbol == NoSymbol) &&
            (symbol.flags.is(Flags.Static)  || symbol.owner.flags.is(Flags.ImplClass)  ||
              /*symbol.annots.find(_ == ctx.definitions.ScalaStaticAnnot)*/ false)

        def isStaticConstructor(implicit ctx: Context): Boolean = {
          (symbol.isStaticMember && symbol.isClassConstructor) || (symbol.name == tpnme.STATIC_CONSTRUCTOR.toString)
        }

        def isInitChild(implicit ctx: Context): Boolean = {
          if (!(symbol.name == "<none>" || symbol == NoSymbol)
              && symbol.owner != NoSymbol) {
            return symbol.owner.name == "<init>" || symbol.owner.isInitChild
          } else {
            return false
          }
        }

        def isWildCard(implicit ctx: Context): Boolean = {
          symbol.name.startsWith(tpnme.WILDCARD.toString) &&
          symbol.name != tpnme.THIS.toString
        }

        def isAnonymousInit(implicit ctx: Context): Boolean = {
          return symbol.owner != NoSymbol &&
          (symbol.owner.isAnonymousFunction || symbol.owner.isAnonymousClass) &&
          symbol.name == "<init>"
        }

        def isUseless(implicit ctx: Context): Boolean = {
          (symbol.name == "<none>" || symbol == NoSymbol) ||
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
          // isSyntheticJavaModule disable the symbol Class in
          // Class.forName(???) to be recorded as Class is considered to
          // be a class in dotty, not a typed.
        }
        def isUseful(implicit ctx: Context): Boolean = !symbol.isUseless
        def isUselessOccurrence(implicit ctx: Context): Boolean = {
          symbol.isUseless &&
          !symbol.isSyntheticJavaModule // references to static Java inner classes should have occurrences
        }
      }

      def resolveClass(symbol: ClassSymbol): Symbol =
        (symbol.companionClass, symbol.companionModule) match {
          case (Some(c), _)                               => c
          case (_, Some(module)) if symbol.flags.is(Flags.Object) => module
          case _                                          => symbol
        }

      def disimbiguate(symbol_path: String, symbol: Symbol): String = {
        try {
          val symbolcl = resolveClass(symbol.owner.asClass)
          symbolcl match {
            case IsClassSymbol(classsymbol) => {
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
        if (symbol.name == "<none>" || symbol.name == "<root>") then {
          ""
        } else {
          val previous_symbol =
            /* When we consider snipper of the form: `abstract class DepAdvD[CC[X[C] <: B], X[Z], C] extends DepTemp`,
              The symbol for C will be something like example/DepAdvD#`<init>`().[CC].[X].[C].
              This is illogic: a init method can't have any child. Thus, when the current symbol is
              a typeparameter, and the owner is an init, we can just "jump" over the init. */
            if (symbol.owner.name == "<init>" && symbol.isType)
              iterateParent(symbol.owner.owner)
            else
              iterateParent(symbol.owner)


          val isdef = symbol match {case IsDefSymbol(_) => true case _ => false}
          val symbolName = if (isMutableAssignement) symbol.trueName + "_=" else symbol.trueName
          val next_atom =
            if (symbol.isPackage) {
              d.Package(symbolName)
            } else if (symbol.isObject) {
              symbol match {
                case IsClassSymbol(classsymbol) =>
                  d.Term(resolveClass(classsymbol).trueName)
                case _ =>
                  d.Term(symbolName)
              }
            } else if (symbol.isValMethod && !symbol.isVarAccessor) {
              d.Term(symbolName)
            } else if (symbol.isMethod || symbol.isUsefulField || symbol.isVarAccessor) {
              d.Method(symbolName,
                       disimbiguate(previous_symbol + symbolName, symbol))
            } else if (symbol.isTypeParameter) {
              d.TypeParameter(symbolName)
            } else if (symbol.isValueParameter) {
              d.Parameter(symbolName)
            } else if (symbol.isType || symbol.isTrait) {
              d.Type(symbolName)
            } else {
              d.Term(symbolName)
            }

          Symbols.Global(previous_symbol, next_atom)
        }
      }

      def symbolToSymbolString(symbol: Symbol, isMutableAssignement:Boolean = false): (String, Boolean) = {
        if (symbol.isSemanticdbLocal) {
          var localsymbol = Symbols.Local(local_offset.toString)
          local_offset += 1
          (localsymbol, false)
        } else {
          (iterateParent(symbol, isMutableAssignement), true)
        }
      }

      def addOccurence(symbol: Symbol,
                       type_symbol: s.SymbolOccurrence.Role,
                       range: s.Range,
                       isMutableAssignement:Boolean = false): Unit = {
        if (symbol.name == "<none>") return

        val symbolName = if (isMutableAssignement) symbol.trueName + "_=" else symbol.trueName
        val (symbol_path, is_global) =
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

        if (symbol_path == "") return
        if (symbol.flags.is(Flags.Synthetic) && type_symbol == s.SymbolOccurrence.Role.DEFINITION) return

        val key = (symbol_path, range)        // this is to avoid duplicates symbols
        // For example, when we define a class as: `class foo(x: Int)`,
        // dotty will generate a ValDef for the x, but the x will also
        // be present in the constructor, thus making a double definition
        if (!symbolPathsMap.contains(key)) {
          symbolPathsMap += key
          occurrences =
            occurrences :+
              s.SymbolOccurrence(
                Some(range),
                symbol_path,
                type_symbol
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
        var localsymbol = Symbols.Local(local_offset.toString)
        local_offset += 1
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
                           force_add: Boolean = false,
                           isMutableAssignement: Boolean = false): Unit = {
        if (tree.symbol.isUseful &&
          tree.symbol.isMutableSetterExplicit(typeSymbol) &&
          (tree.isUserCreated || force_add)) {
          addOccurence(tree.symbol, typeSymbol, range, isMutableAssignement)
        }
      }
      def addOccurenceTypeTree(typetree: TypeTree,
                               type_symbol: s.SymbolOccurrence.Role,
                               range: s.Range): Unit = {
        if (typetree.symbol.isUseful && typetree.isUserCreated) {
          addOccurence(typetree.symbol, type_symbol, range)
        }
      }
      def addOccurencePatternTree(tree: Pattern,
                                  type_symbol: s.SymbolOccurrence.Role,
                                  range: s.Range): Unit = {
        if (tree.symbol.isUseful && tree.isUserCreated) {
          addOccurence(tree.symbol, type_symbol, range)
        }
      }

      def addOccurenceId(parent_path: String, id: Id): Unit = {
        val symbol_path = Symbols.Global(parent_path, d.Term(id.name))
        occurrences =
          occurrences :+
            s.SymbolOccurrence(
              Some(createRange(id.pos)),
              symbol_path,
              s.SymbolOccurrence.Role.REFERENCE
            )
      }

      def createRange(pos: Position): s.Range =
      createRange(pos.startLine, pos.startColumn, pos.endLine, pos.endColumn)

      def createRange(startLine : Int, startColumn : Int, length : Int) : s.Range = {
        createRange(startLine, startColumn, startLine, startColumn + length)
      }

      def createRange(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): s.Range = {
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
          case Term.Select(qualifier, selected) => {
            getImportPath(qualifier)
            rangeSelect(selected, pathTerm.pos)
          }
          case Term.Ident(x) => {
            createRange(pathTerm.pos.startLine, pathTerm.pos.startColumn, pathTerm.symbol.trueName.length)
          }
        }
        addOccurenceTree(pathTerm,
          s.SymbolOccurrence.Role.REFERENCE,
          range)
        iterateParent(pathTerm.symbol)
      }

      def getImportSelectors(parent_path: String,
                             selectors: List[ImportSelector]): Unit = {
        selectors.foreach(selector =>
          selector match {
            case SimpleSelector(id) if id.name != "_" => {
              addOccurenceId(parent_path, id)
            }
            case RenameSelector(id, _) if id.name != "_" => {
              addOccurenceId(parent_path, id)
            }
            case OmitSelector(id) if id.name != "_" => {
              addOccurenceId(parent_path, id)
            }
            case _ =>
        })
      }

      def extractTypeTree(tree: TypeOrBoundsTree) = tree match {
        case IsTypeTree(t) => t
      }

      override def traverseTypeTree(tree: TypeOrBoundsTree)(
          implicit ctx: Context): Unit = {
        tree match {
          case TypeTree.Ident(_) => {
            val typetree = extractTypeTree(tree)
            addOccurenceTypeTree(typetree,
                                 s.SymbolOccurrence.Role.REFERENCE,
                                 createRange(typetree.pos))
          }
          case TypeTree.Select(qualifier, _) => {
            val typetree = extractTypeTree(tree)
            val range = rangeSelect(typetree.symbol.trueName, typetree.pos)
            addOccurenceTypeTree(typetree,
                                 s.SymbolOccurrence.Role.REFERENCE,
                                 range)
            super.traverseTypeTree(typetree)
          }

          case TypeTree.Projection(qualifier, x) => {
              val typetree = extractTypeTree(tree)
              val range = rangeSelect(typetree.symbol.trueName, typetree.pos)
              addOccurenceTypeTree(typetree,
                                  s.SymbolOccurrence.Role.REFERENCE,
                                  range)
              super.traverseTypeTree(typetree)
          }

          case TypeTree.Inferred() => {
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

            val typetree = extractTypeTree(tree)
            val start = typetree.pos.start
            val end = typetree.pos.end
            if (sourceCode.peek(start, end) == typetree.symbol.name) {
              addOccurenceTypeTree(typetree,
                                 s.SymbolOccurrence.Role.REFERENCE,
                                 createRange(typetree.pos))
            }
          }

          case _ => {
            super.traverseTypeTree(tree)
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

      var fittedInitClassRange: Option[s.Range] = None
      var forceAddBecauseParents: Boolean = false
      var classStacks : List[Symbol] = Nil

      def getNumberParametersInit(defdef: DefDef)(implicit ctx: Context): Int = {
        defdef match {
          case DefDef(_, typeParams, paramss, _, _) =>
          paramss.foldLeft(0)((old, c) => old + c.length) + typeParams.length
          case _ => 0
        }
      }

      var disableConstrParamTraversal = false

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

      var isAssignedTerm = false

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case Import(path, selectors) =>
            val key = (tree.symbol.trueName, tree.pos.start)
            if (!package_definitions(key)) {
              package_definitions += key
              getImportSelectors(getImportPath(path), selectors)
            }
          case Term.New(ty) => {
            super.traverseTree(tree)
          }
          case Term.Apply(_, _) => {
            super.traverseTree(tree)
          }
          case ClassDef(classname, constr, parents, derived, selfopt, statements) => {


        val offsetSymbolClass =
          if(tree.symbol.flags.is(Flags.Object)) -1
          else 0
            // we first add the class to the symbol list
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.DEFINITION,
                             createRange(tree.symbol.pos.startLine, tree.symbol.pos.startColumn + offsetSymbolClass, tree.symbol.trueName.length))
            // then the constructor

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

            disableConstrParamTraversal = true
              traverseTree(constr)
            disableConstrParamTraversal = false

            fittedInitClassRange = None

            // we add the parents to the symbol list
            forceAddBecauseParents = !(tree.symbol.flags.is(Flags.Case))
            parents.foreach(_ match {
              case IsTypeTree(t) => traverseTypeTree(t)
              case IsTerm(t) => traverseTree(t)
            })
            forceAddBecauseParents = false
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
                    (old: Int, ct: TermOrTypeTree) =>
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
          }

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

          case DefDef("<init>", _, _, _, _) if tree.symbol.owner.flags.is(Flags.Object) => {
          }

          case Term.Assign(lhs, rhs) => {
            isAssignedTerm = true
            traverseTree(lhs)
            isAssignedTerm = false
            traverseTree(rhs)
          }

          case IsDefinition(cdef) => {
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
            if (tree.symbol.trueName != "<none>") {
              val pos = tree.symbol.pos
              var rangeSymbol = createRange(pos.startLine, pos.startColumn, tree.symbol.trueName.length)

              if (tree.symbol.trueName == "<init>" && sourceCode.peek(pos.start, pos.start + 4) == "this") {
                rangeSymbol = createRange(pos.startLine, pos.startColumn, pos.startColumn + 4)
              }
                addOccurenceTree(tree,
                                 s.SymbolOccurrence.Role.DEFINITION,
                                 rangeSymbol)

            }
            //if (!disableConstrParamTraversal)
            super.traverseTree(cdef)
          }

          case Term.This(Some(id)) => {
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
            /*range = s.Range(tree.pos.startLine, tree.pos.startColumn,
            tree.pos.endLine,
            )*/
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.REFERENCE,
                             rangeThis)
          }

          case Term.Super(_, Some(id)) =>
            {
              addOccurence(classStacks.head,
                s.SymbolOccurrence.Role.DEFINITION,
                createRange(id.pos))
              super.traverseTree(tree)
            }

          case Term.Select(qualifier, _) => {
            var range = rangeSelect(tree.symbol.trueName, tree.pos)

            var shouldForceAdd = false
            if (tree.symbol.trueName == "<init>") {
              if (tree.pos.start == tree.pos.end && sourceCode.peek(tree.pos.start - 5, tree.pos.start - 1) == "this") {
                range = createRange(tree.pos.startLine, tree.pos.start - 5, tree.pos.startLine, 4)
                shouldForceAdd = true
              } else {
                range = createRange(tree.pos.endLine, tree.pos.endColumn, tree.pos.endLine, tree.pos.endColumn)
                shouldForceAdd = qualifier.isUserCreated
              }
            }
            val temp = isAssignedTerm
            isAssignedTerm = false
            super.traverseTree(tree)
            isAssignedTerm = temp
            addOccurenceTree(tree, s.SymbolOccurrence.Role.REFERENCE, range, shouldForceAdd, isAssignedTerm && tree.symbol.flags.is(Flags.Mutable) && !tree.symbol.flags.is(Flags.PrivateLocal))
          }

          case Term.Ident(name) => {
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.REFERENCE,
                             createRange(tree.pos.startLine, tree.pos.startColumn, tree.symbol.trueName.length))

            super.traverseTree(tree)
          }

          case Term.Inlined(Some(c), b, d) => {
            def extractPos(x: TermOrTypeTree) = x match {
              case IsTerm(t) => t.pos
              case IsTypeTree(t) => t.pos
            }
            def extractSymbol(x: TermOrTypeTree) = x match {
              case IsTerm(t) => t.symbol
              case IsTypeTree(t) => t.symbol
            }
            def getPredefFunction(pos: Int): String = {
              sourceCode.peek(pos, pos+1) match {
                case "l" => "locally"
                case "i" => "implicitly"
                case "a" => "assert"
              }
            }
            val parentSymbol = iterateParent(extractSymbol(c))
            if (parentSymbol == "dotty/DottyPredef.") {
              val pos = extractPos(c)
              val function = getPredefFunction(pos.start)
              val range = createRange(pos.startLine, pos.startColumn, pos.startLine, function.length)
              addOccurencePredef(parentSymbol, function, range)
            }

            super.traverseTree(tree)
          }

          case PackageClause(_) =>
            val key = (tree.symbol.trueName, tree.pos.start)
            if (!package_definitions(key)) {
              addOccurenceTree(tree,
                               s.SymbolOccurrence.Role.DEFINITION,
                               createRange(tree.pos.startLine, tree.pos.startColumn + "package ".length, tree.symbol.trueName.length))
              package_definitions += key
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
