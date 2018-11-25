package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
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
        def isTypeParameter: Boolean = symbol match {
          case IsTypeSymbol(_) => symbol.flags.is(Flags.Param)
          case _               => false
        }

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

        // TODO : implement it
        def isJavaClass: Boolean = false

        def isSelfParameter: Boolean =
          symbol != NoSymbol && symbol.owner == symbol

        def isSemanticdbLocal: Boolean = {
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

        def isUseless: Boolean = {
          symbol == NoSymbol ||
          symbol.isAnonymousClass || /*
      sym.isSyntheticConstructor ||
      sym.isStaticConstructor ||
      sym.isLocalChild ||
      sym.isSyntheticValueClassCompanion ||
      sym.isUselessField ||
      sym.isSyntheticCaseAccessor ||*/
          symbol.isRefinementClass /* ||
      sym.isSyntheticJavaModule*/
        }
        def isUseful: Boolean = !symbol.isUseless
        def isUselessOccurrence: Boolean = {
          symbol.isUseless /*&&
      !symbol.isSyntheticJavaModule // references to static Java inner classes should have occurrences
*/
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
              val previous_symbol = iterateParent(symbol.owner)
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
        val symbol_path =
          if (symbol.isSemanticdbLocal) {
            println("LOCAL: ", symbol)
            if (symbolsCache.contains(symbol)) {
              symbolsCache(symbol)
            } else {
              var localsymbol = Symbols.Local(local_offset.toString)
              local_offset += 1
              symbolsCache += (symbol -> localsymbol)
              localsymbol
            }
          } else {
            iterateParent(symbol)
          }

        if (symbol_path == "" || symbol.isUselessOccurrence) return

        occurrences =
          occurrences :+
            s.SymbolOccurrence(
              Some(range),
              symbol_path,
              type_symbol
            )
      }

      def addOccurenceTree(tree: Tree,
                           type_symbol: s.SymbolOccurrence.Role,
                           range: s.Range,
                           force_add: Boolean = false): Unit = {
                             println(tree.symbol)
        if (tree.isUserCreated || force_add) {
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
          case IsPackageClause(tree) => "package ".length
          case _                     => 0
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

      def rangeExclude(range: Position, exclude: Position): s.Range = {
        def max(a: Int, b: Int): Int = { if (a > b) a else b }
        return s.Range(exclude.endLine,
                       exclude.endColumn + 1,
                       range.endLine,
                       range.endColumn)
      }

      def typetreeSymbol(tree: Tree, typetree: TypeTree): Unit =
        typetree match {
          case TypeTree.Inferred => ()
          case _ =>
            addOccurenceTypeTree(
              typetree,
              s.SymbolOccurrence.Role.REFERENCE,
              range(tree, typetree.pos, typetree.symbol.name))
        }

      def getImportPath(path_term: Term): String = {
        path_term match {
          case Term.Select(qualifier, selected) => {
            getImportPath(qualifier)
            val range = rangeExclude(path_term.pos, qualifier.pos)
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

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        //println(tree.pos.startColumn, tree.symbol.name, tree.pos.endColumn)
        tree match {
          case Import(path, selectors) =>
            val key = (tree.symbol.name, tree.pos.start)
            if (!package_definitions(key)) {
              package_definitions += key
              getImportSelectors(getImportPath(path), selectors)
            }
          case IsDefinition(body) => {
            tree match {
              case DefDef(name, _, _, typetree, _) =>
                typetreeSymbol(tree, typetree)
              case ValDef(_, typetree, _) =>
                typetreeSymbol(tree, typetree)
              case _ => ()
            }

            println(tree.symbol.name)
            if (tree.symbol.name != "<none>") {
              val range_symbol = range(tree, tree.symbol.pos, tree.symbol.name)
              /*if (tree.symbol.name == "<init>" && !tree.isUserCreated && !tree.symbol.owner.flags.isObject) {
                val range_symbol2 = s.Range(range_symbol.startLine,
                                            range_symbol.startCharacter - 4,
                                            range_symbol.endLine,
                                            range_symbol.endCharacter - 4)
                addOccurenceTree(tree,
                                 s.SymbolOccurrence.Role.DEFINITION,
                                 range_symbol2,
                                 true)

              } else */ {
                addOccurenceTree(tree,
                                 s.SymbolOccurrence.Role.DEFINITION,
                                 range_symbol)
              }
            }
            super.traverseTree(body)
          }

          case Term.Select(qualifier, _) => {
            val range = rangeExclude(tree.pos, qualifier.pos)
            println(range.startCharacter, range.endCharacter)
            addOccurenceTree(tree, s.SymbolOccurrence.Role.REFERENCE, range)
            super.traverseTree(tree)
          }

          case Term.Ident(_) => {
            // To avoid adding the identifier of the package symbol
            if (tree.symbol.owner.name != "<root>") {
              addOccurenceTree(tree,
                               s.SymbolOccurrence.Role.REFERENCE,
                               range(tree, tree.pos, tree.symbol.name))
            }
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
