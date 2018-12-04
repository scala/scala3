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

class SemanticdbConsumer extends TastyConsumer {
  var stack: List[String] = Nil

  val semantic: s.TextDocument = s.TextDocument()
  var occurrences: Seq[s.SymbolOccurrence] = Seq()

  def toSemanticdb(text: String): s.TextDocument = {
    s.TextDocument(text = text, occurrences = occurrences)
  }
  val package_definitions: Set[Tuple2[String, Int]] = Set()
  var local_offset: Int = 0

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    val symbolsCache: HashMap[Symbol, String] = HashMap()
    val symbolPathsMap: Set[(String, s.Range)] = Set()

    object ChildTraverser extends TreeTraverser {
      var children: List[Tree] = Nil
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit =
        children = tree :: children
      override def traversePattern(pattern: Pattern)(
          implicit ctx: Context): Unit = ()
      override def traverseTypeTree(tree: TypeOrBoundsTree)(
          implicit ctx: Context): Unit = ()
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
    }

    object Traverser extends TreeTraverser {
      implicit class TreeExtender(tree: Tree) {
        def isUserCreated: Boolean = {
          val children: List[Position] =
            ChildTraverser.getChildren(tree)(reflect.rootContext).map(_.pos)
          return !((tree.pos.exists && tree.pos.start == tree.pos.end && children == Nil) || children
            .exists(_ == tree.pos))
        }
      }

      implicit class TypeTreeExtender(tree: TypeTree) {
        def isUserCreated: Boolean = {
          return !(tree.pos.exists && tree.pos.start == tree.pos.end)
        }
      }

      implicit class SymbolExtender(symbol: Symbol) {
        def isClass: Boolean = symbol match {
          case IsClassSymbol(_) => true
          case _                => false
        }

        def isTypeParameter: Boolean = symbol.flags.is(Flags.Param) && symbol.isType

        def isType: Boolean = symbol match {
          case IsTypeSymbol(_) => true
          case _               => false
        }

        def isTerm: Boolean = !symbol.isType

        def isMethod: Boolean = symbol match {
          case IsDefSymbol(_) => true
          case _              => false
        }

        def isPackage: Boolean = symbol match {
          case IsPackageSymbol(_) => true
          case _                  => false
        }

        def isParameter: Boolean = symbol.flags.is(Flags.Param)

        def isObject: Boolean = symbol.flags.is(Flags.Object)

        def isTrait: Boolean = symbol.flags.is(Flags.Trait)

        def isValueParameter: Boolean = symbol.flags.is(Flags.Param)

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
          val isModuleConstructor = symbol.isConstructor && symbol.owner.isClass
          val isTraitConstructor = symbol.isConstructor && symbol.owner.isTrait
          val isInterfaceConstructor = symbol.isConstructor && symbol.owner.flags.is(Flags.JavaDefined)  && symbol.owner.isTrait
          val isEnumConstructor = symbol.isConstructor && symbol.owner.flags.is(Flags.JavaDefined) && symbol.owner.flags.is(Flags.Enum)
          /*val isStaticConstructor = symbol.name == g.TermName("<clinit>")*/
          //val isClassfileAnnotationConstructor = symbol.owner.isClassfileAnnotation
          /*isModuleConstructor || */
          isTraitConstructor || isInterfaceConstructor ||
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
              symbol.asClass.methods.length == 0
            }
          } else {
            false
          }
        }
        def isValMethod(implicit ctx: Context): Boolean = {
          symbol.isMethod && {
            (symbol.flags.is(Flags.FieldAccessor)  && symbol.flags.is(Flags.Stable) ) ||
            (symbol.isUsefulField && !symbol.flags.is(Flags.Mutable) )
          }
        }
        def isScalacField(implicit ctx: Context): Boolean = {
          val isFieldForPrivateThis = symbol.flags.is(Flags.PrivateLocal)  && symbol.isTerm && !symbol.isMethod && !symbol.isObject
          val isFieldForOther = false //symbol.name.endsWith(g.nme.LOCAL_SUFFIX_STRING)
          val isJavaDefined = symbol.flags.is(Flags.JavaDefined)
          (isFieldForPrivateThis || isFieldForOther) && !isJavaDefined
        }
        def isUselessField(implicit ctx: Context): Boolean = {
          symbol.isScalacField && false /*symbol.getterIn(symbol.owner) != g.NoSymbol*/
        }
        def isUsefulField(implicit ctx: Context): Boolean = {
          symbol.isScalacField && !symbol.isUselessField
        }
        def isSyntheticCaseAccessor(implicit ctx: Context): Boolean = {
          symbol.flags.is(Flags.CaseAcessor)  && symbol.name.contains("$")
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

        def isUseless(implicit ctx: Context): Boolean = {
          symbol == NoSymbol ||
          //symbol.isInitChild ||
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

      def resolveClass(symbol: ClassSymbol): Symbol =
        (symbol.companionClass, symbol.companionModule) match {
          case (_, Some(module)) if symbol.flags.is(Flags.Object) => module
          case (Some(c), _) => c
          case _ => symbol
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
            case _ =>
              "()"
          }
        } catch {
          case _ => "()"
        }
      }

      def iterateParent(symbol: Symbol): String = {
        if (symbolsCache.contains(symbol)) {
          return symbolsCache(symbol)
        } else {
          val out_symbol_path =
            if (symbol.name == "<none>" || symbol.name == "<root>") then {
              // TODO had a "NoDenotation" test to avoid
              // relying on the name itself
              ""
            } else {
              val previous_symbol =
                /* When we consider snipper of the form: `abstract class DepAdvD[CC[X[C] <: B], X[Z], C] extends DepTemp`,
              The symbol for C will be something like example/DepAdvD#`<init>`().[CC].[X].[C].
              This is illogic: a init method can't have any child. Thus, when the current symbol is
              a typeparameter (or anything, but here it is just implemented for type parameter), and the owner
              is an init, we can just "jump" over the init. */
                if (symbol.isTypeParameter && symbol.owner.name == "<init>")
                  iterateParent(symbol.owner.owner)
                else
                  iterateParent(symbol.owner)

              val next_atom =
                if (symbol.isPackage) {
                  d.Package(symbol.name)
                } else if (symbol.isObject) {
                  symbol match {
                    case IsClassSymbol(classsymbol) =>
                      d.Term(resolveClass(classsymbol).name)
                    case _ =>
                      d.Term(symbol.name)
                  }
                } else if (symbol.isMethod) {
                  d.Method(symbol.name,
                           disimbiguate(previous_symbol + symbol.name, symbol))
                } else if (symbol.isTypeParameter) {
                  d.TypeParameter(symbol.name)
                } else if (symbol.isValueParameter) {
                  d.Parameter(symbol.name)
                } else if (symbol.isType || symbol.isTrait) {
                  d.Type(symbol.name)
                } else {
                  d.Term(symbol.name)
                }

              Symbols.Global(previous_symbol, next_atom)
            }
          symbolsCache += (symbol -> out_symbol_path)
          out_symbol_path
        }
      }

      def addOccurence(symbol: Symbol,
                       type_symbol: s.SymbolOccurrence.Role,
                       range: s.Range): Unit = {
        val (symbol_path, is_global) =
          if (symbol.isSemanticdbLocal) {
            if (symbolsCache.contains(symbol)) {
              (symbolsCache(symbol), false)
            } else {
              var localsymbol = Symbols.Local(local_offset.toString)
              local_offset += 1
              symbolsCache += (symbol -> localsymbol)
              (localsymbol, false)
            }
          } else {
            (iterateParent(symbol), true)
          }

        println(symbol_path, range)
        if (symbol_path == "" || symbol.isUselessOccurrence) return

        val key = (symbol_path, range)
        if (!is_global || !(symbolPathsMap.contains(key))) {
          if (is_global) {
            symbolPathsMap += key
          }
          occurrences =
            occurrences :+
              s.SymbolOccurrence(
                Some(range),
                symbol_path,
                type_symbol
              )
        }
      }

      def addOccurenceTree(tree: Tree,
                           type_symbol: s.SymbolOccurrence.Role,
                           range: s.Range,
                           force_add: Boolean = false): Unit = {
        if (tree.isUserCreated || (force_add && !(!tree.isUserCreated && iterateParent(tree.symbol) == "java/lang/Object#`<init>`()."))) {
          addOccurence(tree.symbol, type_symbol, range)
        }
      }
      def addOccurenceTypeTree(typetree: TypeTree,
                               type_symbol: s.SymbolOccurrence.Role,
                               range: s.Range): Unit = {
        if (typetree.isUserCreated) {
          addOccurence(typetree.symbol, type_symbol, range)
        }
      }
      def addOccurenceId(parent_path: String, id: Id): Unit = {
        val symbol_path = Symbols.Global(parent_path, d.Term(id.name))
        occurrences =
          occurrences :+
            s.SymbolOccurrence(
              Some(
                s.Range(id.pos.startLine,
                        id.pos.startColumn,
                        id.pos.startLine,
                        id.pos.endColumn)),
              symbol_path,
              s.SymbolOccurrence.Role.REFERENCE
            )
      }

      def range(tree: Tree, pos: Position, name: String): s.Range = {
        val offset = tree match {
          case IsPackageClause(tree)                          => "package ".length
          case IsClassDef(tree) if tree.symbol.flags.is(Flags.Object) => -1
          case _                                              => 0
        }

        val range_end_column =
          if (name == "<init>") {
            pos.endColumn
          } else {
            pos.startColumn + name.length
          }

        s.Range(pos.startLine,
                pos.startColumn + offset,
                pos.startLine,
                range_end_column + offset)
      }

      def rangeSelect(name: String, range: Position): s.Range = {
        val len =
          if (name == "<init>") 0
          else name.length
        return s.Range(range.endLine,
                       range.endColumn - len,
                       range.endLine,
                       range.endColumn)
      }

      def getImportPath(path_term: Term): String = {
        path_term match {
          case Term.Select(qualifier, selected) => {
            getImportPath(qualifier)
            val range = rangeSelect(selected, path_term.pos)
            addOccurenceTree(path_term,
                             s.SymbolOccurrence.Role.REFERENCE,
                             range)
            iterateParent(path_term.symbol)
          }
          case Term.Ident(x) => {
            val range_x = range(path_term, path_term.pos, path_term.symbol.name)
            addOccurenceTree(path_term,
                             s.SymbolOccurrence.Role.REFERENCE,
                             range_x)
            iterateParent(path_term.symbol)
          }
        }
      }

      def getImportSelectors(parent_path: String,
                             selectors: List[ImportSelector]): Unit = {
        selectors.foreach(selector =>
          selector match {
            case SimpleSelector(id) => {
              addOccurenceId(parent_path, id)
            }
            case RenameSelector(id, _) => {
              addOccurenceId(parent_path, id)
            }
            case OmitSelector(id) => {
              addOccurenceId(parent_path, id)
            }
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
                                 s.Range(typetree.pos.startLine,
                                         typetree.pos.startColumn,
                                         typetree.pos.startLine,
                                         typetree.pos.endColumn))
          }
          case TypeTree.Select(qualifier, _) => {
            val typetree = extractTypeTree(tree)
            val range = rangeSelect(typetree.symbol.name, typetree.pos)
            addOccurenceTypeTree(typetree,
                                 s.SymbolOccurrence.Role.REFERENCE,
                                 range)
            super.traverseTypeTree(typetree)
          }
          case _ =>
            super.traverseTypeTree(tree)
        }
      }

      var fittedInitClassRange: Option[s.Range] = None
      var forceAddBecauseParents: Boolean = false

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case Import(path, selectors) =>
            val key = (tree.symbol.name, tree.pos.start)
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
          case ClassDef(classname, constr, parents, selfopt, statements) => {
            // we first add the class to the symbol list
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.DEFINITION,
                             range(tree, tree.symbol.pos, tree.symbol.name))
            println("constr symbol pos: ", constr.symbol.pos.startColumn, constr.symbol.pos.endColumn)
            println("constr pos: ", constr.pos.startColumn, constr.pos.endColumn)
            // then the constructor
            if (!constr.isUserCreated) {
              fittedInitClassRange = Some(
                s.Range(tree.symbol.pos.startLine,
                        tree.symbol.pos.startColumn + classname.length + 1,
                        tree.symbol.pos.startLine,
                        tree.symbol.pos.startColumn + classname.length + 1))
            } else {
              fittedInitClassRange = Some(s.Range(constr.symbol.pos.startLine,
                                            constr.symbol.pos.startColumn,
                                            constr.symbol.pos.endLine,
                                            constr.symbol.pos.endColumn))
            }
            traverseTree(constr)
            fittedInitClassRange = None

            // we add the parents to the symbol list
            forceAddBecauseParents = true
            parents.foreach(_ match {
              case IsTypeTree(t) => traverseTypeTree(t)
              case IsTerm(t)     => traverseTree(t)
            })
            forceAddBecauseParents = false

            selfopt.foreach(traverseTree)


            statements.foreach(traverseTree)
          }
          case IsDefinition(cdef) => {

            if (cdef.symbol.flags.is(Flags.Protected)) {
              cdef.symbol.protectedWithin match {
                case Some(within) => {
                  val startColumn = cdef.pos.startColumn + "protected[".length
                  addOccurence(
                    within.typeSymbol,
                    s.SymbolOccurrence.Role.REFERENCE,
                    s.Range(cdef.pos.startLine,
                            startColumn,
                            cdef.pos.startLine,
                            startColumn + within.typeSymbol.name.length)
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
                    s.Range(cdef.pos.startLine,
                            startColumn,
                            cdef.pos.startLine,
                            startColumn + within.typeSymbol.name.length)
                  )
                }
                case _ =>
              }
            }
            if (tree.symbol.name != "<none>") {
              val range_symbol = range(tree, tree.symbol.pos, tree.symbol.name)
              if (tree.symbol.name == "<init>" && tree.symbol.flags.is(Flags.Object)) {

              } else if (tree.symbol.name == "<init>" && fittedInitClassRange != None) {
                addOccurenceTree(tree,
                                 s.SymbolOccurrence.Role.DEFINITION,
                                 fittedInitClassRange.get,
                                 true)
              } else {
                addOccurenceTree(tree,
                                 s.SymbolOccurrence.Role.DEFINITION,
                                 range_symbol)
              }
            }
            super.traverseTree(cdef)
          }

          case Term.Select(qualifier, _) => {
            val range = {
              val r = rangeSelect(tree.symbol.name, tree.pos)
              if (tree.symbol.name == "<init>")
                s.Range(r.startLine,
                        r.startCharacter + 1,
                        r.endLine,
                        r.endCharacter + 1)
              else r
            }
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.REFERENCE,
                             range,
                             false)
            super.traverseTree(tree)
          }

          case Term.Ident(name) => {
            addOccurenceTree(tree,
                             s.SymbolOccurrence.Role.REFERENCE,
                             range(tree, tree.pos, tree.symbol.name))

            super.traverseTree(tree)
          }

          case PackageClause(_) =>
            val key = (tree.symbol.name, tree.pos.start)
            if (!package_definitions(key)) {
              addOccurenceTree(tree,
                               s.SymbolOccurrence.Role.REFERENCE,
                               range(tree, tree.pos, tree.symbol.name))
              package_definitions += key
            }
            super.traverseTree(tree)

          case tree =>
            super.traverseTree(tree)
        }
      }

    }
    println(root)
    Traverser.traverseTree(root)(reflect.rootContext)
  }

  def println(x: Any): Unit = Predef.println(x)

}
