package dotty.tools
package dotc
package core
package pickling

import ast.Trees._
import PickleFormat._
import core._
import Contexts._, Symbols._, Types._, Names._, Constants._, Decorators._, Annotations._
import collection.mutable
import TastyBuffer._

class TreePickler(pickler: TastyPickler) {
  val buf = new TreeBuffer
  pickler.newSection("ASTs", buf)
  import buf._
  import pickler.nameBuffer.nameIndex
  import ast.tpd._

  private val symRefs = new mutable.HashMap[Symbol, Addr]
  private val forwardSymRefs = new mutable.HashMap[Symbol, List[Addr]]
  private val pickledTypes = new java.util.IdentityHashMap[Type, Any] // Value type is really Addr, but that's not compatible with null

  private def withLength(op: => Unit) = {
    val lengthAddr = reserveRef(relative = true)
    op
    fillRef(lengthAddr, currentAddr, relative = true)
  }

  def registerDef(sym: Symbol) = {
    symRefs(sym) = currentAddr
    forwardSymRefs.get(sym) match {
      case Some(refs) =>
        refs.foreach(fillRef(_, currentAddr, relative = false))
        forwardSymRefs -= sym
      case None =>
    }
  }

  private def pickleName(name: Name) = writeNat(nameIndex(name).index)
  private def pickleName(name: TastyName) = writeNat(nameIndex(name).index)
  private def pickleNameAndSig(name: Name, sig: Signature) = {
    val Signature(params, result) = sig
    pickleName(TastyName.Signed(nameIndex(name), params.map(nameIndex), nameIndex(result)))
  }

  private def pickleSym(sym: Symbol)(implicit ctx: Context) = symRefs.get(sym) match {
    case Some(label) =>
      writeRef(label)
    case None =>
      val ref = reserveRef(relative = false)
      assert(!sym.is(Flags.Package), sym)
      forwardSymRefs(sym) = ref :: forwardSymRefs.getOrElse(sym, Nil)
  }

  def pickle(trees: List[Tree])(implicit ctx: Context) = {
    
    def qualifiedName(sym: Symbol): TastyName =
      if (sym.isRoot || sym.owner.isRoot) TastyName.Simple(sym.name.toTermName)
      else TastyName.Qualified(nameIndex(qualifiedName(sym.owner)), nameIndex(sym.name))
    
    def pickleConstant(c: Constant): Unit = {
      def pickleNum(nonNegTag: Int, negTag: Int) = {
        val x = c.longValue
        if (x < 0) {
          writeByte(negTag)
          writeLongNat(-(x + 1))
        }
        else {
          writeByte(nonNegTag)
          writeLongNat(x)
        }
      }
      c.tag match {
        case UnitTag => 
          writeByte(UNITconst)
        case BooleanTag => 
          writeByte(if (c.booleanValue) TRUEconst else FALSEconst)
        case ByteTag => 
          writeByte(BYTEconst)
          writeInt(c.byteValue)
        case ShortTag => 
          writeByte(SHORTconst)
          writeInt(c.shortValue)
        case CharTag => 
          writeByte(CHARconst)
          writeNat(c.charValue)
        case IntTag => 
          writeByte(INTconst)
          writeInt(c.intValue)
        case LongTag => 
          writeByte(LONGconst)
          writeLongInt(c.longValue)
        case FloatTag => 
          writeByte(FLOATconst)
          writeInt(java.lang.Float.floatToRawIntBits(c.floatValue))
        case DoubleTag =>
          writeByte(DOUBLEconst)
          writeLongInt(java.lang.Double.doubleToRawLongBits(c.doubleValue))
        case StringTag =>
          writeByte(STRINGconst)
          writeNat(nameIndex(c.stringValue).index)
        case NullTag => 
          writeByte(NULLconst)
        case ClazzTag =>
          writeByte(CLASSconst)
          withLength { pickleType(c.typeValue) }
        case EnumTag =>
          writeByte(ENUMconst)
          withLength { pickleType(c.symbolValue.termRef) }          
      }
    }

    def pickleType(tpe0: Type, richTypes: Boolean = false): Unit = try {
      val tpe = tpe0.stripTypeVar
      val prev = pickledTypes.get(tpe)
      if (prev == null) {
        pickledTypes.put(tpe, currentAddr)
        pickleNewType(tpe, richTypes)
      }
      else {
        writeByte(SHARED)
        writeRef(prev.asInstanceOf[Addr])
      }
    } catch {
      case ex: AssertionError =>
        println(i"error when pickling type $tpe0")
        throw ex
    }
      
    def pickleNewType(tpe: Type, richTypes: Boolean): Unit = tpe match {
      case ConstantType(value) => 
        pickleConstant(value)
      case tpe: WithFixedSym =>
        val sym = tpe.symbol
        if (sym.isStatic && sym.isType || sym.is(Flags.Module)) {
          writeByte(if (tpe.isType) TYPEREFstatic else TERMREFstatic)
          pickleName(qualifiedName(sym))
        } 
        else if (tpe.prefix == NoPrefix) {
          def pickleRef() = {
            writeByte(if (tpe.isType) TYPEREFdirect else TERMREFdirect)
            pickleSym(sym)        
          }
          if (sym is Flags.BindDefinedType) {
            registerDef(sym)
            writeByte(BIND)
            withLength {
              pickleName(sym.name)
              pickleType(sym.info)
              pickleRef()
            }
          }
          else pickleRef()
        }
        else {
          writeByte(if (tpe.isType) TYPEREFsymbol else TERMREFsymbol)
          pickleSym(sym); pickleType(tpe.prefix)
        }
      case tpe: TermRefWithSignature =>
        writeByte(TERMREF)
        pickleNameAndSig(tpe.name, tpe.signature); pickleType(tpe.prefix)
      case tpe: NamedType =>
        writeByte(if (tpe.isType) TYPEREF else TERMREF)
        pickleName(tpe.name); pickleType(tpe.prefix)
      case tpe: ThisType =>
        writeByte(THIS)
        withLength { pickleType(tpe.tref) }
      case tpe: SuperType =>
        writeByte(SUPERtype)
        withLength { pickleType(tpe.thistpe); pickleType(tpe.supertpe)}
      case tpe: SkolemType =>
        writeByte(SKOLEMtype)
        writeRef(pickledTypes.get(tpe).asInstanceOf[Addr])
      case tpe: RefinedType =>
        val args = tpe.argInfos(interpolate = false)
        if (args.isEmpty) {
          writeByte(REFINEDtype)
          withLength { pickleName(tpe.refinedName); pickleType(tpe.refinedInfo, richTypes = true) }
        }
        else {
          writeByte(APPLIEDtype)
          withLength { pickleType(tpe.withoutArgs(args)); args.foreach(pickleType(_)) }
        }
      case tpe: TypeAlias =>
        writeByte(TYPEALIAS)
        withLength { pickleType(tpe.alias, richTypes) }
      case tpe: TypeBounds =>
        writeByte(TYPEBOUNDS)
        withLength { pickleType(tpe.lo, richTypes); pickleType(tpe.hi, richTypes) }
      case tpe: AnnotatedType =>
        writeByte(ANNOTATED)
        withLength { pickleTree(tpe.annot.tree); pickleType(tpe.tpe, richTypes) }
      case tpe: AndOrType =>
        writeByte(if (tpe.isAnd) ANDtype else ORtype)
        withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
      case tpe: ExprType =>
        writeByte(BYNAMEtype)
        withLength { pickleType(tpe.underlying) }
      case tpe: MethodType if richTypes =>
        writeByte(METHODtype)
        pickleMethodic(tpe.resultType, tpe.paramNames, tpe.paramTypes)
      case tpe: PolyType if richTypes =>
        writeByte(POLYtype)
        pickleMethodic(tpe.resultType, tpe.paramNames, tpe.paramBounds)
      case tpe: PolyParam => 
        if (!pickleParamType(tpe))
          // TODO figure out why this case arises in e.g. pickling AbstractFileReader.
          ctx.typerState.constraint.entry(tpe) match {
            case TypeBounds(lo, hi) if lo eq hi => pickleNewType(lo, richTypes)
            case _ => assert(false, s"orphan poly parameter: $tpe")
          }
      case tpe: MethodParam =>
        assert(pickleParamType(tpe), s"orphan method parameter: $tpe")
      case tpe: LazyRef =>
        pickleType(tpe.ref)
      case NoType =>
        writeByte(NOTYPE)
//      case NoPrefix =>    // not sure we need this!
//        writeByte(NOPREFIX)
    }
    
    def pickleMethodic(result: Type, names: List[Name], types: List[Type]) = 
      withLength {
        pickleType(result, richTypes = true)
        (names, types).zipped.foreach { (name, tpe) =>
          pickleName(name); pickleType(tpe)       
        }
      }
    
    def pickleParamType(tpe: ParamType): Boolean = {
      val binder = pickledTypes.get(tpe.binder)
      val pickled = binder != null
      if (pickled) {
        writeByte(PARAMtype)
        withLength { writeRef(binder.asInstanceOf[Addr]); writeNat(tpe.paramNum) }
      }
      pickled
    }
    
    def pickleTpt(tpt: Tree): Unit = pickleType(tpt.tpe) // TODO correlate with original when generating positions
    
    def pickleTreeIfNonEmpty(tree: Tree): Unit = 
      if (!tree.isEmpty) pickleTree(tree)

    def pickleTree(tree: Tree): Unit = try {
      pickledTrees.put(tree, currentAddr)
      tree match {
      case Ident(name) =>
        tree.tpe match {
          case tp: TermRef => pickleType(tp)
          case _ => 
             writeByte(IDENT)
             pickleName(name)
             pickleType(tree.tpe)
        }
      case This(_) => 
        pickleType(tree.tpe)
      case Select(qual, name) => 
        writeByte(SELECT)
        val sig = tree.tpe.signature
        if (sig == Signature.NotAMethod) pickleName(name)
        else pickleNameAndSig(name, sig)
        pickleTree(qual)
      case Apply(fun, args) =>
        writeByte(APPLY)
        withLength {
          pickleTree(fun)
          args.foreach(pickleTree)
        }
      case TypeApply(fun, args) =>
        writeByte(TYPEAPPLY)
        withLength {
          pickleTree(fun)
          args.foreach(pickleTpt)
        }
      case Literal(const) =>
        pickleConstant(const)
      case Super(qual, mix) =>
        writeByte(SUPER)
        withLength { 
          pickleTree(qual);
          if (!mix.isEmpty) {
            val SuperType(_, mixinType) = tree.tpe
            pickleType(mixinType)
          }
        }
      case New(tpt) =>
        writeByte(NEW)
        withLength { pickleTpt(tpt) }
      case Pair(left, right) =>
        writeByte(PAIR)
        withLength { pickleTree(left); pickleTree(right) }
      case Typed(expr, tpt) =>
        writeByte(TYPED)
        withLength { pickleTree(expr); pickleTpt(tpt) }
      case NamedArg(name, arg) =>
        writeByte(NAMEDARG)
        withLength { pickleName(name); pickleTree(arg) }
      case Assign(lhs, rhs) =>
        writeByte(ASSIGN)
        withLength { pickleTree(lhs); pickleTree(rhs) }
      case Block(stats, expr) =>
        writeByte(BLOCK)
        withLength { pickleTree(expr); stats.foreach(pickleTree) }
      case If(cond, thenp, elsep) =>
        writeByte(IF)
        withLength{ pickleTree(cond); pickleTree(thenp); pickleTree(elsep) }
      case Closure(env, meth, tpt) => 
        writeByte(CLOSURE)
        withLength{ pickleTree(meth); pickleTpt(tpt); env.foreach(pickleTree) }
      case Match(selector, cases) =>
        writeByte(MATCH)
        withLength { pickleTree(selector); cases.foreach(pickleTree) }
      case CaseDef(pat, guard, rhs) =>
        writeByte(CASEDEF)
        withLength { pickleTree(pat); pickleTree(rhs); pickleTreeIfNonEmpty(guard) }
      case Return(expr, from) =>
        writeByte(RETURN)
        withLength { pickleSym(from.symbol); pickleTreeIfNonEmpty(expr) }
      case Try(block, cases, finalizer) =>
        writeByte(TRY)
        withLength { pickleTree(block); cases.foreach(pickleTree); pickleTreeIfNonEmpty(finalizer) }
      case Throw(expr) =>
        writeByte(THROW)
        withLength { pickleTree(expr) }
      case SeqLiteral(elems) =>
        writeByte(REPEATED)
        withLength { elems.foreach(pickleTree) }
      case TypeTree(original) =>
        pickleTpt(tree)
      case Bind(name, body) =>
        registerDef(tree.symbol)
        writeByte(BIND)
        withLength { pickleName(name); pickleType(tree.symbol.info); pickleTree(body) }
      case Alternative(alts) =>
        writeByte(ALTERNATIVE)
        withLength { alts.foreach(pickleTree) }
      case UnApply(fun, implicits, patterns) =>
        writeByte(UNAPPLY)
        withLength { 
          pickleTree(fun)
          for (implicitArg <- implicits) {
            writeByte(IMPLICITARG)
            withLength { pickleTree(implicitArg) }
          }
          pickleType(tree.tpe)
          patterns.foreach(pickleTree) 
        }
      case tree: ValDef =>
        pickleDef(VALDEF, tree.symbol, tree.tpt, tree.rhs)
      case tree: DefDef =>
        def pickleParams = {
          tree.tparams.foreach(pickleParam)
          for (vparams <- tree.vparamss) {
            writeByte(PARAMS)
            withLength { vparams.foreach(pickleParam) }
          }          
        }
        pickleDef(DEFDEF, tree.symbol, tree.tpt, tree.rhs, pickleParams)
      case tree: TypeDef =>
        pickleDef(TYPEDEF, tree.symbol, tree.rhs)
      case tree: Template =>
        registerDef(tree.symbol)
        writeByte(TEMPLATE)
        val (params, rest) = tree.body span {
          case stat: TypeDef => stat.mods is Flags.Param
          case stat: ValDef => stat.mods is Flags.ParamAccessor
          case _ => false
        }
        withLength {
          params.foreach(pickleParam)
          tree.parents.foreach(pickleTree)
          if (!tree.self.isEmpty)
            pickleDef(SELFDEF, tree.self.symbol, tree.self.tpt)
          pickleTree(tree.constr)
          rest.foreach(pickleTree)
        }
      case Import(expr, selectors) =>
        writeByte(IMPORT)
        withLength {
          pickleTree(expr)
          selectors foreach {
            case Pair(Ident(from), Ident(to)) => 
              writeByte(RENAMED)
              withLength { pickleName(from); pickleName(to) }
            case Ident(name) =>
              writeByte(IMPORTED)
              withLength { pickleName(name) }
          }
        }
      case PackageDef(pid, stats) =>
        writeByte(PACKAGE)
        withLength { pickleType(pid.tpe); stats.foreach(pickleTree) }
      case EmptyTree =>
        writeByte(EMPTYTREE)
    }}
    catch {
      case ex: AssertionError =>
        println(i"error when pickling tree $tree")
        throw ex
    }

    
    def pickleDef(tag: Int, sym: Symbol, tpt: Tree, rhs: Tree = EmptyTree, pickleParams: => Unit = ()) = {
      registerDef(sym)
      writeByte(tag)
      withLength {
        pickleName(sym.name)
        pickleParams
        tpt match {
          case tpt: TypeTree => pickleTpt(tpt)
          case _ => pickleTree(tpt)
        }
        if (tag == VALDEF || tag == DEFDEF) pickleTree(rhs)
        pickleModifiers(sym)
      }
    }
    
    def pickleParam(tree: Tree): Unit = tree match {
      case tree: ValDef => pickleDef(PARAM, tree.symbol, tree.tpt)
      case tree: TypeDef => pickleDef(TYPEPARAM, tree.symbol, tree.rhs)      
    }

    def pickleModifiers(sym: Symbol): Unit = {
      import Flags._
      val flags = sym.flags
      val privateWithin = sym.privateWithin
      if (privateWithin.exists) {
        writeByte(if (flags is Protected) PROTECTEDqualified else PRIVATEqualified)
        withLength { pickleType(privateWithin.typeRef) }
      }
      if (flags is Private) writeByte(PRIVATE)
      if (flags is Protected) if (!privateWithin.exists) writeByte(PROTECTED)
      if (flags is Final) writeByte(FINAL)
      if (flags is Case) writeByte(CASE)
      if (flags is Override) writeByte(OVERRIDE)
      if (flags is Inline) writeByte(INLINE)
      if (flags is JavaStatic) writeByte(STATIC)
      if (flags is Module) writeByte(MODULE)
      if (flags is Local) writeByte(LOCAL)
      if (flags is Synthetic) writeByte(SYNTHETIC)
      if (flags is Artifact) writeByte(ARTIFACT)
      if (flags is Scala2x) writeByte(SCALA2X) 
      if (flags is InSuperCall) writeByte(INSUPERCALL)
      if (sym.isTerm) {
        if (flags is Implicit) writeByte(IMPLICIT)
        if (flags is Lazy) writeByte(LAZY)
        if (flags is AbsOverride) writeByte(ABSOVERRIDE)
        if (flags is Mutable) writeByte(MUTABLE)
        if (flags is Accessor) writeByte(FIELDaccessor)
        if (flags is ParamAccessor) writeByte(PARAMaccessor)
        if (flags is CaseAccessor) writeByte(CASEaccessor)  
        if (flags is DefaultParameterized) writeByte(DEFAULTparameterized)
        if (flags is DefaultInit) writeByte(DEFAULTinit)
      } else {
        if (flags is Sealed) writeByte(SEALED)
        if (flags is Abstract) writeByte(ABSTRACT)      
        if (flags is Covariant) writeByte(COVARIANT)
        if (flags is Contravariant) writeByte(CONTRAVARIANT)
      }
      sym.annotations.foreach(pickleAnnotation)
    }
    
    def pickleAnnotation(ann: Annotation) = {
      writeByte(ANNOTATION)
      withLength { pickleType(ann.symbol.typeRef); pickleTree(ann.tree) }
    }

    trees.foreach(pickleTree)
    assert(forwardSymRefs.isEmpty, i"unresolved symbols: ${forwardSymRefs.keySet.toList}%, %")
    compactify()
  }  
}
