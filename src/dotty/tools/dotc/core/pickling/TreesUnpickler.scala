package dotty.tools
package dotc
package core
package pickling

import Contexts._, Symbols._, Types._, Scopes._, SymDenotations._, Names._, NameOps._
import StdNames._, Denotations._, Flags._, Constants._, Annotations._
import util.Positions._
import dotty.tools.dotc.ast.{tpd, Trees, untpd}
import Trees._
import Decorators._
import TastyUnpickler._, TastyBuffer._
import annotation.switch
import scala.collection.{ mutable, immutable }
import typer.Mode

/** Unpickler for typed trees
 *  @param reader         the reader from which to unpickle
 *  @param tastyName      the nametable
 *  @param roots          a set of pre-existing symbols whose attributes should be overwritten
 *                        instead of creating a new symbol.
 *  @param readPositions  a flag indicating whether positions should be read
 */
class TreesUnpickler(reader: TastyReader, tastyName: TastyName.Table,
                     roots: Set[SymDenotation], readPositions: Boolean) {
  import dotty.tools.dotc.core.pickling.PickleFormat._
  import TastyName._
  import tpd._

  private val rootOwner = if (roots.isEmpty) NoSymbol else roots.head.owner
  private val symAtAddr  = new mutable.HashMap[Addr, Symbol]
  private val treeAtAddr = new mutable.HashMap[Addr, Tree]
  
  private val typeAtAddr = new mutable.HashMap[Addr, Type] // currently populated only for types that are known to be SHAREd.

  def unpickle()(implicit ctx: Context): List[Tree] =
    new TreeReader(reader).readTopLevelStats()
    
  def toTermName(tname: TastyName): TermName = tname match {
    case Simple(name) => name
    case Qualified(qual, name) => toTermName(qual) ++ "." ++ toTermName(name)
    case Signed(original, params, result) => toTermName(original)
    case Expanded(original) => ???
    case ModuleClass(original) => toTermName(original).moduleClassName.toTermName
    case SuperAccessor(accessed) => ???
    case DefaultGetter(meth, num) => ???
  }

  def toTermName(ref: NameRef): TermName = toTermName(tastyName(ref))
  def toTypeName(ref: NameRef): TypeName = toTermName(ref).toTypeName

  class Completer(reader: TastyReader) extends LazyType {
    import reader._
    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
      treeAtAddr(currentAddr) = new TreeReader(reader).readIndexedDef()
    }
  }

  class TreeReader(val reader: TastyReader) {
    import reader._
        
    def forkAt(start: Addr) = new TreeReader(subReader(start, endAddr))
    def fork = forkAt(currentAddr)
    
    def skipTree(tag: Int): Unit = {
      // println(s"skipping ${astTagToString(tag)} at $currentAddr")
      if (tag >= firstLengthTreeTag) skipTo(readEnd())
      else if (tag >= firstNatASTTreeTag) { readNat(); skipTree() }
      else if (tag >= firstNatTreeTag) readNat()
    }
    def skipTree(): Unit = skipTree(readByte())
    
    def skipParams(): Unit =
      while (nextByte == PARAMS || nextByte == TYPEPARAM) skipTree()

    def readName(): TermName = toTermName(readNameRef())

    def readNameSplitSig()(implicit ctx: Context): Any /* TermName | (TermName, Signature) */ =
      tastyName(readNameRef()) match {
        case Signed(original, params, result) =>
          var sig = Signature(params map toTypeName, toTypeName(result))
          if (sig == Signature.NotAMethod) sig = Signature.NotAMethod
          (toTermName(original), sig)
        case name =>
          toTermName(name)
      }
    
// ------ Reading types -----------------------------------------------------
    
    /** Read names in an interleaved sequence of (parameter) names and types/bounds */
    def readParamNames[N <: Name](end: Addr): List[N] = 
      until(end) { try readName().asInstanceOf[N] finally skipTree() }

    /** Read types or bounds in an interleaved sequence of (parameter) names and types/bounds */
    def readParamTypes[T <: Type](end: Addr)(implicit ctx: Context): List[T] = 
      until(end) { readNat(); readType().asInstanceOf[T] }

    /** Read referece to definition and return symbol created at that definition */
    def readSymRef()(implicit ctx: Context): Symbol = symAtAddr(readAddr())

    /** Read a type */
    def readType()(implicit ctx: Context): Type = {
      val start = currentAddr
      // println(s"read type at $start")
      val tag = readByte()

      def registeringType[T](tp: Type, op: => T): T = {
        typeAtAddr(start) = tp
        op
      }
            
      def readLengthType(): Type = {
        val end = readEnd()
        
        def readNamesSkipParams[N <: Name]: (List[N], TreeReader) = {
          val nameReader = fork
          nameReader.skipTree() // skip result
          val paramReader = nameReader.fork
          (nameReader.readParamNames[N](end), paramReader)
        }
        
        try { 
          (tag: @switch) match {
            case THIS =>
              ThisType.raw(readType().asInstanceOf[TypeRef])
            case SUPERtype =>
              SuperType(readType(), readType())
            case REFINEDtype =>
              val parent = readType()
              var name: Name = readName()
              if (nextByte == TYPEBOUNDS || nextByte == TYPEALIAS) name = name.toTypeName
              RefinedType(parent, name, registeringType(_, readType()))
            case APPLIEDtype =>
              readType().appliedTo(until(end)(readType()))
            case TYPEBOUNDS =>
              TypeBounds(readType(), readType())
            case TYPEALIAS =>
              TypeAlias(readType())
            case ANNOTATED =>
              AnnotatedType(Annotation(readTerm()), readType())
            case ANDtype =>
              AndType(readType(), readType())
            case ORtype =>
              OrType(readType(), readType())
            case BIND =>
              symAtAddr(start) = 
                ctx.newSymbol(ctx.owner, readName().toTypeName, BindDefinedType, readType())
              readType()
            case BYNAMEtype =>
              ExprType(readType())
            case POLYtype =>
              val (names, paramReader) = readNamesSkipParams[TypeName]
              try PolyType(names)(
                registeringType(_, paramReader.readParamTypes[TypeBounds](end)),
                _ => readType())
              finally skipTo(end)
            case METHODtype =>
              val (names, paramReader) = readNamesSkipParams[TermName]
              try MethodType(names, paramReader.readParamTypes[Type](end))(
                registeringType(_, readType()))
              finally skipTo(end)
            case PARAMtype =>
              readTypeRef() match {
                case binder: PolyType => PolyParam(binder, readNat())
                case binder: MethodType => MethodParam(binder, readNat())
              }
            case CLASSconst =>
              readEnd()
              ConstantType(Constant(readType()))
            case ENUMconst =>
              readEnd()
              ConstantType(Constant(readTermRef().termSymbol))
          }
        }
        finally assert(currentAddr == end)
      }
      
      def readSimpleType(): Type = (tag: @switch) match {
        case TYPEREFdirect | TERMREFdirect => 
          NamedType.withFixedSym(NoPrefix, readSymRef())
        case TYPEREFsymbol | TERMREFsymbol =>
          val sym = readSymRef()
          NamedType.withFixedSym(readType(), sym)
        case TYPEREFpkg =>
          val name = readName()
          val pkg = 
            if (name == nme.ROOT) defn.RootClass
            else if (name == nme.EMPTY_PACKAGE) defn.EmptyPackageClass
            else ctx.requiredPackage(name).moduleClass
          pkg.typeRef
        case TERMREFpkg =>
          val name = readName()
          val pkg = 
            if (name == nme.ROOT) defn.RootPackage
            else if (name == nme.EMPTY_PACKAGE) defn.EmptyPackageVal
            else ctx.requiredPackage(name)
          pkg.termRef
        case TYPEREF =>
          val name =  readName().toTypeName
          TypeRef(readType(), name)
        case TERMREF =>
          readNameSplitSig() match {
            case name: TermName => TermRef.all(readType(), name)
            case (name: TermName, sig: Signature) => TermRef.withSig(readType(), name, sig)
          }
        case SKOLEMtype =>
          SkolemType(readTypeRef())
        case NOTYPE =>
          NoType
        case SHARED =>
          val ref = readAddr()
          typeAtAddr.getOrElseUpdate(ref, forkAt(ref).readType())
        case UNITconst =>
          ConstantType(Constant(()))
        case TRUEconst =>
          ConstantType(Constant(true))
        case FALSEconst =>
          ConstantType(Constant(false))
        case BYTEconst =>
          ConstantType(Constant(readInt().toByte))
        case SHORTconst =>
          ConstantType(Constant(readInt().toShort))
        case CHARconst =>
          ConstantType(Constant(readNat().toChar))
        case INTconst =>
          ConstantType(Constant(readInt()))
        case LONGconst =>
          ConstantType(Constant(readLongInt()))
        case FLOATconst =>
          ConstantType(Constant(java.lang.Float.intBitsToFloat(readInt())))
        case DOUBLEconst =>
          ConstantType(Constant(java.lang.Double.longBitsToDouble(readLongInt())))
        case STRINGconst =>
          ConstantType(Constant(readName().toString))
        case NULLconst =>
          ConstantType(Constant(null))
      }
      
      if (tag < firstLengthTreeTag) readSimpleType() else readLengthType()
    }
    
    def readTypeRef(): Type = 
      typeAtAddr(readAddr())

    def readPath()(implicit ctx: Context): Type = {
      val tp = readType()
      assert(tp.isInstanceOf[SingletonType])
      tp
    }
    
    def readTermRef()(implicit ctx: Context): TermRef = 
      readType().asInstanceOf[TermRef]

// ------ Reading definitions -----------------------------------------------------

    /** Create symbol of definition node and enter in symAtAddr map */
    def createSymbol()(implicit ctx: Context): Unit = {
      val start = currentAddr
      // println(s"creating symbol at $start")
      val tag = readByte()
      val end = readEnd()
      val name = if (tag == TYPEDEF || tag == TYPEPARAM) readName().toTypeName else readName()
      skipParams()
      val isAbstractType = nextByte == TYPEBOUNDS
      val isClass = nextByte == TEMPLATE
      val templateStart = currentAddr
      skipTree()
      val rhsIsEmpty = 
        (tag == VALDEF || tag == DEFDEF) && {
          val result = nextByte == EMPTYTREE
          skipTree()
          result
        }
      val (givenFlags, annots, privateWithin) = readModifiers(end)
      val lacksDefinition =
        rhsIsEmpty && !name.isConstructorName && !givenFlags.is(ParamOrAccessor) ||
        isAbstractType
      var flags = givenFlags
      if (lacksDefinition) flags |= Deferred
      if (ctx.mode.is(Mode.InSuperCall) && !flags.is(ParamOrAccessor)) flags |= InSuperCall
      if (ctx.owner.isClass) {
        if (tag == TYPEPARAM) flags |= Param
        else if (tag == PARAM) flags |= ParamAccessor // TODO: try to unify param and paramaccessor
      }
      else if (isParamTag(tag)) flags |= Param
      val nameMatches = (_: Denotation).symbol.name == name
      val isRoot = ctx.owner == rootOwner && roots.exists(nameMatches)
      val completer = 
        if (isRoot) new Completer(subReader(start, end)) with SymbolLoaders.SecondCompleter
        else new Completer(subReader(start, end))
      def adjustIfModule(completer: Completer) = 
        if (flags is Module) ctx.adjustModuleCompleter(completer, name) else completer
      val sym =
        if (isRoot) {
          val d = roots.find(nameMatches).get
          d.info = completer
          d.setFlag(flags)
          d.privateWithin = privateWithin
          d.symbol
        } else if (isClass)
          ctx.newClassSymbol(ctx.owner, name.asTypeName, flags, completer, privateWithin, coord = start.index)
        else {
          ctx.newSymbol(ctx.owner, name, flags, completer, privateWithin, coord = start.index)
        } // TODO set position
      sym.annotations = annots
      ctx.enter(sym)
      symAtAddr(start) = sym
      if (isClass) {
        completer.withDecls(newScope)
        forkAt(templateStart).indexTemplateParams()(ctx.fresh.setOwner(sym))
      }
    }

    /** Read modifier list into triplet of flags, annotations and a privateWithin 
     *  boindary symbol.
     */
    def readModifiers(end: Addr)(implicit ctx: Context): (FlagSet, List[Annotation], Symbol) = {
      var flags: FlagSet = EmptyFlags
      var annots = new mutable.ListBuffer[Annotation]
      var privateWithin: Symbol = NoSymbol
      while (currentAddr.index != end.index) {
        def addFlag(flag: FlagSet) = {
          flags |= flag
          readByte()
        }
        nextByte match {
          case PRIVATE => addFlag(Private)
          case INTERNAL => ??? // addFlag(Internal)
          case PROTECTED => addFlag(Protected)
          case ABSTRACT => addFlag(Abstract)
          case FINAL => addFlag(Final)
          case SEALED => addFlag(Sealed)
          case CASE => addFlag(Case)
          case IMPLICIT => addFlag(Implicit)
          case LAZY => addFlag(Lazy)
          case OVERRIDE => addFlag(Override)
          case INLINE => addFlag(Inline)
          case ABSOVERRIDE => addFlag(AbsOverride)
          case STATIC => addFlag(JavaStatic)
          case MODULE => addFlag(Module)
          case LOCAL => addFlag(Local)
          case SYNTHETIC => addFlag(Synthetic)
          case ARTIFACT => addFlag(Artifact)
          case MUTABLE => addFlag(Mutable)
          case LABEL => addFlag(Label)
          case FIELDaccessor => addFlag(Accessor)
          case PARAMaccessor => addFlag(ParamAccessor)
          case CASEaccessor => addFlag(CaseAccessor)
          case COVARIANT => addFlag(Covariant)
          case CONTRAVARIANT => addFlag(Contravariant)
          case SCALA2X => addFlag(Scala2x)
          case DEFAULTparameterized => addFlag(DefaultParameterized)
          case DEFAULTinit => addFlag(DefaultInit)
          case INSUPERCALL => addFlag(InSuperCall)
          case PRIVATEqualified => 
            readByte()
            readEnd()
            privateWithin = readType().typeSymbol
          case PROTECTEDqualified =>
            addFlag(Protected)
            readEnd()
            privateWithin = readType().typeSymbol
          case ANNOTATION =>
            readByte()
            val end = readEnd()
            val sym = readType().typeSymbol
            val lazyAnnotTree = readLater(end, _.readTerm())
            annots += Annotation.deferred(sym, _ => lazyAnnotTree.complete)
          case _ =>
            assert(false, s"illegal modifier tag at $currentAddr")
        }
      }
      (flags, annots.toList, privateWithin)
    }
    
    /** Create symbols for a definitions in statement sequence between
     *  current address and `end`.
     */
    def indexStats(end: Addr)(implicit ctx: Context) =
      until(end) { if (isDefTag(nextByte)) createSymbol() else skipTree() }

    /** Create symbols the longest consecutive sequence of parameters with given
     *  `tag starting at current address.
     */
    def indexParams(tag: Int)(implicit ctx: Context) =
      while (nextByte == tag) createSymbol()
      
    /** Create symbols for all type and value parameters of template starting
     *  at current address.
     */
    def indexTemplateParams()(implicit ctx: Context) = {
      assert(readByte() == TEMPLATE)
      readEnd()
      indexParams(TYPEPARAM)
      indexParams(PARAM)
    }

    /** If definition was already read by a completer, return the previously read tree
     *  or else read definition. 
     */
    def readIndexedDef()(implicit ctx: Context): Tree = treeAtAddr.remove(currentAddr) match {
      case Some(tree) => skipTree(); tree
      case none => readNewDef()
    }
    
    private def readNewDef()(implicit ctx: Context): Tree = {
      val start = currentAddr
      // println(s"reading def at $start")
      val sym = symAtAddr(start)
      val tag = readByte()
      val end = readEnd()

      def readParams[T <: MemberDef](tag: Int)(implicit ctx: Context): List[T] = {
        fork.indexParams(tag)
        readIndexedParams(tag)
      }
    
      def readParamss(implicit ctx: Context): List[List[ValDef]] = {
        collectWhile(nextByte == PARAMS) {
          readByte()
          readEnd()
          readParams[ValDef](PARAM)
        }
      }
      
      def readRhs(implicit ctx: Context) = readLater(end, _.readTerm())

      def localCtx = ctx.fresh.setOwner(sym)
      
      def ta =  ctx.typeAssigner

      readName()
      val tree = tag match {
        case DEFDEF =>
          val tparams = readParams[TypeDef](TYPEPARAM)(localCtx)
          val vparamss = readParamss(localCtx)
          val tpt = readTpt()
          val typeParams = tparams.map(_.symbol)
          val valueParamss = vparamss.nestedMap(_.symbol)
          val resType = ctx.effectiveResultType(sym, typeParams, tpt.tpe)
          sym.info = ctx.methodType(typeParams, valueParamss, resType)
          ta.assignType(
            untpd.DefDef(
              sym.name.asTermName, tparams, vparamss, tpt, readRhs(localCtx)),
            sym)
        case VALDEF =>
          sym.info = readType()
          ValDef(sym.asTerm, readRhs(localCtx))
        case TYPEDEF | TYPEPARAM =>
          if (sym.isClass)
            ta.assignType(
              untpd.TypeDef(sym.name.asTypeName, readTemplate(localCtx)),
              sym)
          else {
            sym.info = readType()
            TypeDef(sym.asType)
          }
        case PARAM | SELFDEF =>
          sym.info = readType()
          ValDef(sym.asTerm)
      }
      skipTo(end)
      addAddr(start, tree)
    }

    def readTemplate(implicit ctx: Context): Template = {
      val start = currentAddr
      val cls = ctx.owner.asClass
      val localDummy = ctx.newLocalDummy(cls)
      assert(readByte() == TEMPLATE)
      val end = readEnd()
      readIndexedParams(TYPEPARAM)
      readIndexedParams(PARAM)
      val parents = collectWhile(nextByte != SELFDEF && nextByte != DEFDEF) {
        nextByte match {
          case APPLY | TYPEAPPLY => readTerm()
          case _ => readTpt()
        }
      }
      fork.indexStats(end)
      val self = if (nextByte == SELFDEF) readIndexedDef().asInstanceOf[ValDef] else EmptyValDef
      val constr = readIndexedDef().asInstanceOf[DefDef]
      val lazyStats = readLater(end, _.readIndexedStats(localDummy, end))
      addAddr(start,
        untpd.Template(constr, parents, self, lazyStats)
          .withType(localDummy.nonMemberTermRef))
    }
    
    def readIndexedStat(exprOwner: Symbol)(implicit ctx: Context): Tree = nextByte match {
      case TYPEDEF | VALDEF | DEFDEF | IMPORT => readIndexedDef()
      case _ => readTerm()(ctx.withOwner(exprOwner))
    }
    
    def readStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] = {
      fork.indexStats(end)
      readIndexedStats(exprOwner, end)
    } 
    
    def readIndexedStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] =
      until(end)(readIndexedStat(exprOwner))
    
    def readIndexedParams[T <: MemberDef](tag: Int)(implicit ctx: Context): List[T] =
      collectWhile(nextByte == tag) { readIndexedDef().asInstanceOf[T] }

// ------ Reading terms -----------------------------------------------------

    def readTerm()(implicit ctx: Context): Tree = {
      val start = currentAddr
      // println(s"reading term at $start")
      val tag = readByte()

      def readSimpleTerm(): Tree = tag match {
        case IDENT =>
          untpd.Ident(readName()).withType(readType())
        case SELECT =>
          def readQual(name: Name) = {
            val localCtx = 
              if (name == nme.CONSTRUCTOR) ctx.fresh.addMode(Mode.InSuperCall) else ctx
            readTerm()(localCtx)
          }
          readNameSplitSig match {
            case name: Name => readQual(name).select(name)
            case (name: Name, sig: Signature) => readQual(name).selectWithSig(name, sig)
          }
        case EMPTYTREE =>
          EmptyTree
        case _ =>
          ref(readTermRef())
      }

      def readLengthTerm(): Tree = {
        val end = readEnd()

        try {
          (tag: @switch) match {
            case SUPER => 
              val qual = readTerm()
              val mixClass = ifBefore(end)(readType().typeSymbol, NoSymbol)
              val mixName = if (mixClass.exists) mixClass.name.asTypeName else tpnme.EMPTY
              tpd.Super(qual, mixName, ctx.mode.is(Mode.InSuperCall), mixClass)
            case APPLY => 
              val fn = readTerm()
              val isJava = fn.tpe.isInstanceOf[JavaMethodType]
              def readArg() = readTerm() match {
                case SeqLiteral(elems) if isJava => JavaSeqLiteral(elems)
                case arg => arg
              }
              tpd.Apply(fn, until(end)(readArg()))
            case TYPEAPPLY =>
              tpd.TypeApply(readTerm(), until(end)(readTpt()))
            case NEW =>
              New(readTpt())
            case PAIR =>
              Pair(readTerm(), readTerm())
            case TYPED =>
              Typed(readTerm(), readTpt())
            case NAMEDARG =>
              NamedArg(readName(), readTerm())
            case ASSIGN =>
              Assign(readTerm(), readTerm())
            case BLOCK =>
              val exprReader = fork
              skipTree()
              Block(readStats(ctx.owner, end), exprReader.readTerm())(ctx.fresh.setNewScope)
            case IF =>
              If(readTerm(), readTerm(), readTerm())
            case CLOSURE =>
              val meth = readTerm()
              val tpt = readTpt()
              Closure(until(end)(readTerm()), meth, tpt)
            case MATCH =>
              Match(readTerm(), readCases())
            case RETURN =>
              val from = readSymRef()
              val expr = ifBefore(end)(readTerm(), EmptyTree)
              Return(expr, Ident(from.termRef))
            case TRY =>
              Try(readTerm(), readCases(), ifBefore(end)(readTerm(), EmptyTree))
            case THROW =>
              Throw(readTerm())
            case REPEATED =>
              SeqLiteral(until(end)(readTerm()))
            case BIND =>
              val name = readName()
              val info = readType()
              val sym = ctx.newSymbol(ctx.owner, name, EmptyFlags, info)
              symAtAddr(start) = sym
              Bind(sym, readTerm())
            case ALTERNATIVE =>
              Alternative(until(end)(readTerm()))
            case UNAPPLY =>
              val fn = readTerm()
              val implicitArgs = 
                collectWhile(nextByte == IMPLICITARG) {
                  readByte()
                  readEnd()
                  readTerm()
                }
              val patType = readType()
              val argPats = until(end)(readTerm())
              UnApply(fn, implicitArgs, argPats, patType)
          }
        }
        finally assert(currentAddr == end, s"$currentAddr $end ${astTagToString(tag)}")
      }

      addAddr(start, 
        if (tag < firstLengthTreeTag) readSimpleTerm()
        else readLengthTerm())
    }
    
    def readTpt()(implicit ctx: Context) = {
      val start = currentAddr
      addAddr(start, TypeTree(readType()))
    }

    def readCases()(implicit ctx: Context): List[CaseDef] = 
      collectWhile(nextByte == CASEDEF) {
        val start = currentAddr
        readByte()
        val end = readEnd()
        val pat = readTerm()
        val rhs = readTerm()
        val guard = ifBefore(end)(readTerm(), EmptyTree)
        addAddr(start, CaseDef(pat, guard, rhs))
      }

    def readTopLevelStats()(implicit ctx: Context): List[Tree] = {
      fork.indexStats(endAddr)
      until(endAddr) {
        if (nextByte == PACKAGE) {
          val start = currentAddr
          readByte()
          val end = readEnd()
          val pid = ref(readTermRef()).asInstanceOf[RefTree]
          addAddr(start,
            PackageDef(pid, readStats(NoSymbol, end)(ctx.fresh.setOwner(pid.symbol))))     
        }
        else readIndexedStat(ctx.owner)
      }
    }

    def readLater[T](end: Addr, op: TreeReader => T): Trees.Lazy[T] = {
      val localReader = fork
      skipTo(end)
      new Trees.Lazy[T] { def complete: T = op(localReader) }
    }
    
// ------ Hooks for positions ------------------------------------------------
    
    /** A temporary position encoding.
     *  Start and end fields are each given the address of bytes from which tree is unpickled
     *  These are later overridden with the actual offsets taken from the Positions section.
     */
    def indexPosition(addr: Addr): Position = {
      assert(addr.index < MaxOffset)
      Position(addr.index, addr.index, 0)
    }
      
    /** Record address from which tree was created as a temporary position in the tree. */
    def addAddr[T <: Tree](addr: Addr, tree: T): T = {
      if (readPositions) { tree.setPosUnchecked(indexPosition(addr)); tree }
      else tree
    }
  }
}
