package dotty.tools.dotc
package transform
package linker


import dotty.tools.dotc.FromTasty.TASTYCompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.backend.jvm.CollectEntryPoints
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.{Flags, Hashable, TypeErasure}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform.CollectSummaries.SubstituteByParentMap
import dotty.tools.dotc.transform.Summaries._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeTransforms._

import collection.{immutable, mutable}
import collection.mutable.{LinkedHashMap, LinkedHashSet, TreeSet}
import scala.annotation.tailrec

class CollectSummaries extends MiniPhase { thisTransform =>
  import tpd._
  import Summaries._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "summaries"

  val treeTransform: TreeTransform = new Collect

  private var methodSums = List[MethodSummary]()
  private var noSummaryAvailable = Set[Symbol]()

   /*
  def getSummary(d: Symbol)(implicit ctx: Context): Option[MethodSummary] = {
    if (noSummaryAvailable(d)) None
    else methodSums.find(_.methodDef == d).orElse {
      val nw = retrieveSummary(d)
      methodSums = nw ::: methodSums
      nw.headOption
    }
  }

  private def retrieveSummary(claz: Symbol)(implicit ctx: Context): List[MethodSummary] = {
    val topDenot = claz.topLevelClass.denot.asSymDenotation
    topDenot match {
      case clsd: ClassDenotation =>
        clsd.initInfo match {
          case info: ClassfileLoader =>
            info.load(clsd) match {
              case Some(unpickler: DottyUnpickler) =>
                class STreeUnpickler(reader: TastyReader, tastyName: TastyName.Table) extends TreeUnpickler(reader, tastyName) {

                  roots = Set.empty

                  def getStartReader: Option[TreeReader] = {
                    val st = new TreeReader(reader)
                    st.skipToplevel()(ctx.addMode(Mode.AllowDependentFunctions))

                    while (true) {
                      while (reader.nextByte != TastyFormat.VALDEF && !reader.isAtEnd) st.skipTree()
                      if (reader.isAtEnd) return None // no section here
                      val tag = reader.readByte()
                      val end = reader.readEnd()
                      val name = st.readName()
                      if (name.toString == sectionName + unpickler.unpickler.uuid) return Some(st.forkAt(end))
                      st.skipTree() // skip type
                      st.skipTree() // skip rhs
                    }

                    None
                  }

               }
                class STreeSectionUnpickler extends TreeSectionUnpickler {
                  override def unpickle(reader: TastyReader, tastyName: Table): STreeUnpickler = {
                      new STreeUnpickler(reader, tastyName)
                  }
                }

                val tastySection = unpickler.unpickler.unpickle(new STreeSectionUnpickler).get
                val treeReader = tastySection.asInstanceOf[STreeUnpickler].getStartReader.get

                val unp = new SectionUnpickler[List[MethodSummary]](sectionName) {
                  def unpickle(reader: TastyReader, tastyName: Table): List[MethodSummary] = {
                    def readSymbolRef = {
                      val s = treeReader.readType()
                      s.termSymbol.orElse(s.typeSymbol).orElse(s.classSymbol)
                    }

                    def readType = treeReader.readType()

                    def readMS: MethodSummary = {
                      val sym = readSymbolRef
                      val methodsSz = reader.readInt()

                      val methodsCalled = new mutable.HashMap[Type, List[CallInfo]]()

                      for(_ <- 0 until methodsSz) {
                        val reciever = readType
                        val listSz = reader.readInt()

                        def readCallInfo: CallInfo = {
                          val t = readType
                          val targsSz = reader.readByte()
                          val targs = for(_ <- 0 until targsSz) yield readType
                          val argsSz = reader.readByte()
                          val argsPassed = for(_ <- 0 until argsSz) yield readSymbolRef
                          new CallInfo(t, targs.toList, argsPassed.toList)
                        }

                        val calls = for(_ <- 0 until listSz) yield readCallInfo
                        methodsCalled(reciever) = calls.toList
                      }

                      val accessedModulesSz = reader.readInt()

                      val accesedModules = for(_ <- 0 until accessedModulesSz) yield readSymbolRef

                      val argumentReturned = reader.readByte()

                      val bitsExtected =
                        sym.info.widenDealias.asInstanceOf[MethodType].paramTypess.foldLeft(0)(_+_.size) + 2 // this and thisAccessed
                      val bytesExpected = bitsExtected / 8 + (if(bitsExtected % 8 > 0) 1 else 0)
                      val bytes = reader.readBytes(bytesExpected)
                      val (thisAccessed :: argumentStoredToHeap) = bytes.toList.flatMap{ bt =>
                        List((bt & 1)  != 0, (bt & 2)  != 0, (bt & 4)  != 0, (bt & 8)   != 0,
                             (bt & 16) != 0, (bt & 32) != 0, (bt & 64) != 0, (bt & 128) != 0)
                      }

                      new MethodSummary(sym, thisAccessed, methodsCalled, accesedModules.toList, argumentReturned.toByte, argumentStoredToHeap.take(bitsExtected - 1))
                    }

                    val version = reader.readInt()

                    val methodsSz = reader.readInt()

                    val methods = for(_ <- 0 until methodsSz) yield readMS

                    methods.toList
                  }
                }
                unpickler.unpickler.unpickle(unp).getOrElse(Nil)
              case _ => Nil
            }
        }
    }
  }  */

  def methodSummaries = methodSums

  class Collect extends TreeTransform {
    def phase = thisTransform

    var methodSumarries = List[MethodSummary]()
    var methodSummaryStack = mutable.Stack[MethodSummary]()
    var curMethodSummary: MethodSummary = null

    override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
      if (ctx.compilationUnit.isInstanceOf[TASTYCompilationUnit])
        NoTransform // will retrieve them lazily
      else this
    }


    override def prepareForDefDef(tree: tpd.DefDef)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if(!(sym is Flags.Label)) {
        methodSummaryStack.push(curMethodSummary)
        val args = tree.vparamss.flatten.map(_.symbol) // outer param for constructors
        curMethodSummary = new MethodSummary(sym,
          false,
          mutable.Map.empty,
          Nil,
          -1,
          (0 to args.length).map(_ => true).toList
        )
      }
      this
    }


    override def prepareForValDef(tree: tpd.ValDef)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if (sym.exists && ((sym.is(Flags.Lazy) &&  (sym.owner.is(Package) || sym.owner.isClass)) ||  //lazy vals and modules
        sym.owner.name.startsWith(nme.LOCALDUMMY_PREFIX) || // blocks inside constructor
        sym.owner.isClass)) { // fields
        // owner is a template
        methodSummaryStack.push(curMethodSummary)
        curMethodSummary = new MethodSummary(sym,
          false,
          mutable.Map.empty,
          Nil,
          -1,
          List(true)
        )
      }
      this
    }



    override def prepareForTemplate(tree: tpd.Template)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if(!(sym is Flags.Label)) {
        methodSummaryStack.push(curMethodSummary)
        curMethodSummary = new MethodSummary(sym,
          false,
          mutable.Map.empty,
          Nil,
          -1,
          List(true)
        )
      }
      this
    }


    override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if(!(sym is Flags.Label)) {
        assert(curMethodSummary.methodDef eq tree.symbol)
        methodSumarries = curMethodSummary :: methodSumarries
        curMethodSummary = methodSummaryStack.pop()
      }
      tree
    }

    override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
//      if (tree.name.toString.contains("Iterable"))
//        println("hoo")
      if(!(tree.symbol is Flags.Label)) {
        assert(curMethodSummary.methodDef eq tree.symbol)
        methodSumarries = curMethodSummary :: methodSumarries
        curMethodSummary = methodSummaryStack.pop()
      }
      tree
    }

    override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if(sym.exists && ((sym.is(Flags.Lazy) &&  (sym.owner.is(Package) || sym.owner.isClass)) ||  //lazy vals and modules
        sym.owner.name.startsWith(nme.LOCALDUMMY_PREFIX) || // blocks inside constructor
        sym.owner.isClass)) { // fields
        assert(curMethodSummary.methodDef eq tree.symbol)

        methodSumarries = curMethodSummary :: methodSumarries
        curMethodSummary = methodSummaryStack.pop()
      }
      tree
    }

    override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if(sym.isClass) {
        val isEntryPoint = CollectEntryPoints.isJavaEntryPoint(sym)
        /*summaries = ClassSummary(sym.asClass,
          methodSumarries
        ) :: summaries
        methodSumarries = Nil*/
      }
      tree
    }

    def registerModule(sym: Symbol)(implicit ctx: Context): Unit = {
      if((curMethodSummary ne null) && (sym is Flags.ModuleVal)) {
        curMethodSummary.accessedModules = sym :: curMethodSummary.accessedModules
        registerModule(sym.owner)
      }
      val res = sym.info.finalResultType.termSymbol
      if ((curMethodSummary ne null) && (res is Flags.ModuleVal)) {
        curMethodSummary.accessedModules = res :: curMethodSummary.accessedModules
        registerModule(res.owner)
      }

    }

    def registerCall(tree: Tree)(implicit ctx: Context): Unit = {

      def symbolOf(t: Tree) = {
        val s = t.symbol.orElse(t.tpe.classSymbol).orElse(TypeErasure.erasure(t.tpe).classSymbol)
        assert(s.exists)
        s
      }

      def receiverArgumentsAndSymbol(t: Tree, accArgs: List[List[Tree]] = Nil, accT: List[Tree] = Nil):
      (Tree, Tree, List[List[Tree]], List[Tree], Type) = t match {
        case Block(stats, expr) => receiverArgumentsAndSymbol(expr, accArgs, accT)
        case TypeApply(fun, targs) if fun.symbol eq t.symbol => receiverArgumentsAndSymbol(fun, accArgs, targs)
        case Apply(fn, args) if fn.symbol == t.symbol => receiverArgumentsAndSymbol(fn, args :: accArgs, accT)
        case Select(qual, _) =>
          (qual, t, accArgs, accT, t.tpe)
        case x: This => (x, x, accArgs, accT, x.tpe)
        case x => (x, x, accArgs, accT, x.tpe)
      }
      val widenedTp = tree.tpe.widen
      if ((widenedTp.isInstanceOf[MethodicType]) && (!tree.symbol.exists || tree.symbol.info.isInstanceOf[MethodicType])) return;
      val (reciever, call, arguments, typeArguments, method) = receiverArgumentsAndSymbol(tree)

      val storedReciever = reciever.tpe

      assert(storedReciever.exists)

      def skipBlocks(s: Tree): Tree = {
        s match {
          case s: Block => skipBlocks(s.expr)
          case _ => s
        }
      }

      val args: List[Type] = arguments.flatten.map(x =>  skipBlocks(x) match {
        case exp: Closure =>
          val SAMType(e) =  exp.tpe
          new ClosureType(exp, x.tpe, e.symbol, null.asInstanceOf[OuterTargs])
        case Select(New(tp),_) => new PreciseType(tp.tpe)
        case Apply(Select(New(tp), _), args) => new PreciseType(tp.tpe)
        case Apply(TypeApply(Select(New(tp), _), targs), args) => new PreciseType(tp.tpe)
        case _ =>
          x.tpe match {
            case _ if x.isInstanceOf[NamedArg] => ref(symbolOf(x.asInstanceOf[NamedArg].arg)).tpe
            case _ => x.tpe
      }})

      // Create calls to wappXArray for varArgs
      val repeatedArgsCalls = tree match {
        case Apply(fun, _) if fun.symbol.info.isVarArgsMethod =>
          @tailrec def refine(tp: Type): Type = tp match {
            case tp: TypeAlias   => refine(tp.alias.dealias)
            case tp: RefinedType => refine(tp.refinedInfo)
            case _               => tp
          }
          @tailrec def getVarArgTypes(tp: Type, acc: List[Type] = Nil): List[Type] = tp match {
            case tp: PolyType => getVarArgTypes(tp.resultType, acc)
            case tp @ MethodType(_, paramTypes) if paramTypes.nonEmpty && paramTypes.last.isRepeatedParam =>
              getVarArgTypes(tp.resultType, refine(paramTypes.last) :: acc)
            case _ => acc
          }

          def wrapArrayTermRef(arrayName: TermName) =
            TermRef(defn.ScalaPredefModuleRef, defn.ScalaPredefModule.requiredMethod(arrayName))

          getVarArgTypes(fun.tpe.widenDealias).map { tp =>
            val args = List(defn.ArrayOf(tp))
            val sym = tp.typeSymbol
            if (sym.isTypeParam || sym == defn.NothingClass)
              CallInfo(wrapArrayTermRef(nme.genericWrapArray), List(tp), args)
            else if (defn.isPrimitiveClass(sym))
              CallInfo(wrapArrayTermRef(nme.wrapXArray(sym.name)), Nil, args)
            else
              CallInfo(wrapArrayTermRef(nme.wrapRefArray), List(tp), args)
          }

        case _ => Nil
      }

      val fillInStackTrace = tree match {
        case Apply(Select(newThrowable, nme.CONSTRUCTOR), _) if newThrowable.tpe.derivesFrom(defn.ThrowableClass) =>
          List(CallInfo(TermRef(newThrowable.tpe, newThrowable.tpe.widenDealias.classSymbol.requiredMethod("fillInStackTrace")), Nil, Nil))
        case _ => Nil
      }

      def initialValuesFor(tpe: Type): List[CallInfo] = {
        tpe.widenDealias.classSymbol.mixins.flatMap {
          _.info.decls.collect {
            case decl if !decl.is(Flags.Method) && decl.isTerm =>
              CallInfo(new TermRefWithFixedSym(tpe, decl.name.asTermName, decl.symbol.asTerm), Nil, Nil)
          }
        }
      }
      val initialValues = tree match {
        case Apply(TypeApply(Select(qualifier, nme.CONSTRUCTOR), _), _) => initialValuesFor(qualifier.tpe)
        case Apply(Select(qualifier, nme.CONSTRUCTOR), _) => initialValuesFor(qualifier.tpe)
        case _ => Nil
      }

      val javaAccessible = tree match {
        case Apply(fun, args) if fun.symbol.is(Flags.JavaDefined) && !fun.symbol.is(Flags.Deferred) =>
          for {
            (paramType, argType) <- fun.tpe.widenDealias.paramTypess.flatten.zip(args.map(_.tpe))
            if !defn.isPrimitiveClass(paramType.classSymbol)
            decl <- paramType.decls // FIXME paramType with some type argument is equivalent to argType as such it exposes all decls of the argType
            if decl.isTerm && !decl.isConstructor
            if decl.name != nme.isInstanceOf_ && decl.name != nme.asInstanceOf_ && decl.name != nme.synchronized_
          } yield {
            val call =
              TermRef(argType, argType.widenDealias.classSymbol.requiredMethod(decl.name.asTermName, decl.info.paramTypess.flatten))
            val targs = call.widenDealias match {
              case call: PolyType => call.paramBounds.map(_.hi)
              case _ => Nil
            }
            CallInfo(call, targs, decl.info.paramTypess.flatten)
          }

        case _ => Nil
      }

      val languageDefinedCalls = repeatedArgsCalls ::: fillInStackTrace ::: initialValues ::: javaAccessible

      curMethodSummary.methodsCalled(storedReciever) = CallInfo(method, typeArguments.map(_.tpe), args) :: languageDefinedCalls ::: curMethodSummary.methodsCalled.getOrElse(storedReciever, Nil)
    }

    override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Flags.Package)) {
        registerModule(tree.symbol)
      }
      val select = tree.tpe match {
        case TermRef(prefix: TermRef, name) =>
          Some(tpd.ref(prefix).select(tree.symbol))
        case TermRef(prefix: ThisType, name) =>
          Some(tpd.This(prefix.cls).select(tree.symbol))
        case TermRef(NoPrefix, name) =>
          if (tree.symbol is Flags.Method) { // todo: this kills dotty {
            val widenedTp = tree.tpe.widen
            if ((widenedTp.isInstanceOf[MethodicType]) && (!tree.symbol.exists || tree.symbol.info.isInstanceOf[MethodicType]))
              return tree;
            registerCall(tree)
            return tree;
            // Some(This(tree.symbol.topLevelClass.asClass).select(tree.symbol)) // workaround #342 todo: remove after fixed
          }
          else None
        case _ => None
      }

      select.map(transformSelect)

      tree
    }

    override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Flags.Package | Flags.Label)) {
        registerModule(tree.symbol)
        registerCall(tree)
      }
      // handle nullary methods
      tree
    }

    override def transformThis(tree: tpd.This)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      curMethodSummary.thisAccessed = true
      tree
    }

    override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Flags.Label))
       registerCall(tree)
      tree
    }

    override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      registerCall(tree)
      tree
    }

    override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {

      //println("hoho")
      methodSums = methodSumarries

      /*

      for { cls <- ctx.compilationUnit.picklers.keySet} {
        def serializeCS(methods: List[MethodSummary], pickler: TastyPickler): Unit = {
          val buf = new TastyBuffer(5000)
          val treePickl = pickler.treePkl
          val anchorTree = tpd.SyntheticValDef((sectionName + pickler.uuid.toString).toTermName, Literal(Constant(sectionName)))

          treePickl.preRegister(anchorTree)
          treePickl.pickle(anchorTree :: Nil)

          pickler.newSection(sectionName, buf)
          val start = treePickl.buf.currentAddr
          buf.writeInt(version)//1

          def writeSymbolRef(sym: Symbol) = {
            treePickl.pickleType(ref(sym).tpe)
          }
          def writeTypeRef(tp: Type) = {
            treePickl.pickleType(tp)
          }


          def serializeMS(ms: MethodSummary) = {
            writeSymbolRef(ms.methodDef) //26

            buf.writeInt(ms.methodsCalled.size) //29
            for ((reciever, methods) <- ms.methodsCalled) {
              writeTypeRef(reciever) //36
              buf.writeInt(methods.size)

              def writeCallInfo(c: CallInfo): Unit = {
                writeTypeRef(c.call)
                buf.writeByte(c.targs.size)
                c.targs foreach writeTypeRef

                buf.writeByte(c.argumentsPassed.size)
                c.argumentsPassed foreach writeSymbolRef
              }
              methods foreach writeCallInfo
            }

            buf.writeInt(ms.accessedModules.length)
            ms.accessedModules foreach writeSymbolRef

            buf.writeByte(ms.argumentReturned)
            (ms.thisAccessed :: ms.argumentStoredToHeap).grouped(8).map(_.foldRight(0) { (bl: Boolean, acc: Int) =>
              (if (bl) 1 else 0) + 2 * acc
            }) foreach (buf.writeByte)
          }

          buf.writeInt(methods.length) // 19

          methods foreach serializeMS

          val sz = treePickl.buf.currentAddr.index - start.index

          ctx.debuglog("new section for " + cls + " size:"
            + sz + "/" + buf.currentAddr + "increased by " + (sz + buf.length) * 1.0 / start.index)
          // note: this is huge overestimate. This section contains a lot of refferences to already existing symbols and types
          // and will be compressed during bytecode generation by TreePickler.compactify
        }

          val s = methodSumarries.filter(_.methodDef.topLevelClass == cls)

          // println(s)

          serializeCS(s, ctx.compilationUnit.picklers(cls))
      } */



      tree
    }
  }
}

object Summaries {
  val version: Int = 1
  val sectionName = "$ummaries"


   class ClosureType(val meth: tpd.Closure, val u: Type, val implementedMethod: Symbol, val outerTargs: OuterTargs)(implicit ctx: Context) extends SingletonType {
     /** The type to which this proxy forwards operations. */
     def underlying(implicit ctx: Context): Type = u

     /** customized hash code of this type.
       * NotCached for uncached types. Cached types
       * compute hash and use it as the type's hashCode.
       */
     override val hash: Int = implementedMethod.hashCode() + meth.meth.symbol.hashCode()


     override def hashCode() = hash


     override def equals(other: Any): Boolean = other match {
       case that: ClosureType =>
           meth == that.meth &&
           u == that.u &&
           implementedMethod == that.implementedMethod
       case _ => false
     }

   }

   class PreciseType(u: Type) extends SingletonType {

     /** customized hash code of this type.
       * NotCached for uncached types. Cached types
       * compute hash and use it as the type's hashCode.
       */
     def hash: Int = {
       val underlying = u.hash
       if (underlying == Hashable.NotCached) Hashable.NotCached
       else if (underlying == Hashable.NotCached - 1) underlying
       else underlying + 1
     }

     def underlying(implicit ctx: Context): Type = u
   }

   class ErazedType extends UncachedProxyType {
     /** The type to which this proxy forwards operations. */
     def underlying(implicit ctx: Context): Type = ctx.definitions.AnyType
   }


  case class CallInfo(call: Type, // this is type of method, that includes full type of reciever, eg: TermRef(reciever, Method)
                       targs: List[Type],
                       argumentsPassed: List[Type]
                       )(implicit ctx: Context) {
    call.widenDealias match {
      case t: PolyType => assert(t.paramNames.size == targs.size)
      case _ =>
    }
  }

  final case class OuterTargs(mp: Map[Symbol, Map[Name, Type]]) extends AnyVal {
    def ++(parent: (Symbol, List[Type]))(implicit ctx: Context): OuterTargs = {
      parent._2.foldLeft(this)((x, y) => x.+(parent._1, y))
    }
    def +(parent: (Symbol, Type))(implicit ctx: Context): OuterTargs = {
      this.+(parent._1, parent._2)
    }
    def +(parent: Symbol, tp: Type)(implicit ctx: Context): OuterTargs = {
      this.+(parent, tp.typeSymbol.name, tp)
    }
    def +(parent: Symbol, name: Name, tp: Type): OuterTargs = {
      val old = mp.getOrElse(parent, Map.empty)
      new OuterTargs(mp.updated(parent, old + (name -> tp)))
    }
    def nonEmpty = mp.nonEmpty
    def ++(other: OuterTargs)(implicit ctx: Context) = {
      other.mp.foldLeft(this) { (x, y) =>
        y._2.foldLeft(x: OuterTargs)((x: OuterTargs, z: (Name, Type)) => x.+(y._1, z._1, z._2))
      }
    }
    def combine(environment: OuterTargs)(implicit ctx: Context): OuterTargs = {
      val subst = new SubstituteByParentMap(environment)
      val newMap = mp.map(x => (x._1, x._2.map(x => (x._1, subst.apply(x._2)))))
      OuterTargs(newMap)
    }
  }

  object OuterTargs {
    def empty = new OuterTargs(Map.empty)
  }

  private var nextCallId = 0

  class CallWithContext(call: Type, targs: List[Type], argumentsPassed: List[Type], val outerTargs: OuterTargs,
                        val parent: CallWithContext, val callee: CallInfo)(implicit ctx: Context) extends CallInfo(call, targs, argumentsPassed) {

    val id = { nextCallId += 1; nextCallId }

    val outEdges = mutable.HashMap[CallInfo, List[CallWithContext]]().withDefault(x => Nil)

    override def hashCode(): Int = super.hashCode() ^ outerTargs.hashCode()

    override def equals(obj: scala.Any): Boolean = {
      obj match {
        case t: CallWithContext => t.call == this.call && t.targs == this.targs && this.argumentsPassed == t.argumentsPassed &&  outerTargs == obj.asInstanceOf[CallWithContext].outerTargs
        case _ => false
      }
    }
  }

  class TypeWithContext(val tp: Type, val outerTargs: OuterTargs) {
    val castsCache: mutable.Set[Cast] = mutable.Set.empty

    override def hashCode(): Int = tp.hashCode() * 31 + outerTargs.hashCode()

    override def equals(obj: scala.Any): Boolean = obj match {
      case t: TypeWithContext => t.tp.equals(tp) && (t.outerTargs equals outerTargs)
      case _ => false
    }
  }

  case class Cast(from: Type, to: Type)(implicit ctx: Context) {
    override def equals(other: scala.Any): Boolean = {
      other match {
        case Cast(a, b) =>
          a =:= from && b =:= to
        case _ => false
      }
    }

    override def hashCode(): Int =
      from.typeSymbol.hashCode() * 31 + to.typeSymbol.hashCode()
  }


  case class MethodSummary(methodDef: Symbol,
                           var thisAccessed: Boolean,
                           methodsCalled: mutable.Map[Type, List[CallInfo]],
                           // allocatedLambdas
                           var accessedModules: List[Symbol],
                           argumentReturned: Byte, // -1 if not known
                           var argumentStoredToHeap: List[Boolean] // not currently collected
                          )

  def simplifiedClassOf(t: Type)(implicit ctx: Context) = {
    val s = t.widenDealias.finalResultType.classSymbol.orElse(TypeErasure.erasure(t.widenDealias.finalResultType).classSymbol)
    assert(s.exists)
    s
  }
}

class BuildCallGraph extends Phase {
  private var reachableMethods: Set[CallWithContext] = null
  private var reachableTypes: Set[TypeWithContext] = null
  private var casts: Set[Cast] = null
  private var outerMethods: Set[Symbol] = null

  def getReachableMethods = reachableMethods
  def getReachableTypes   = reachableTypes
  def getReachableCasts   = casts
  def getOuterMethods     = outerMethods
  def phaseName: String = "callGraph"
  def isEntryPoint(s: Symbol)(implicit ctx: Context): Boolean = {
    (s.name eq nme.main) /* for speed */  && s.is(Method) && CollectEntryPoints.isJavaMainMethod(s)
  }

  class Worklist[A] {
    val reachableItems = mutable.Set[A]()
    var newItems = immutable.Set[A]()

    def +=(elem: A) = {
      // No new elements are accepted if they've already been reachable before
      if (!reachableItems(elem)) {
        newItems += elem
        reachableItems += elem
      }
    }

    /**
     * Add new items to the worklist. It also adds the items to the reachable set.
     */
    def ++=(xs: TraversableOnce[A]): this.type = { xs.seq foreach +=; this }

    /**
     * Clear the new items
     */
    def clear = {
      newItems = immutable.Set[A]()
    }

    /**
     * Do we have new items to process?
     */
    def nonEmpty = {
      newItems.nonEmpty
    }

    /**
     * How many new items do we have?
     */
    def size = {
      newItems.size
    }
  }

  final val AnalyseOrig = 1
  final val AnalyseTypes = 2
  final val AnalyseArgs = 3

  def parentRefinements(tp: Type)(implicit ctx: Context): OuterTargs =
    new TypeAccumulator[OuterTargs]() {
      def apply(x: OuterTargs, tp: Type): OuterTargs = tp match {
        case t: RefinedType =>
          val member = t.parent.member(t.refinedName).symbol
          val parent = member.owner
          val tparams = parent.info.typeParams
          val id = tparams.indexOf(member)
          // assert(id >= 0) // TODO: IS this code needed at all?

          val nlist = x +(parent, t.refinedName, t.refinedInfo)
          apply(nlist, t.parent)
        case _ =>
          foldOver(x, tp)
      }
    }.apply(OuterTargs.empty, tp)

  /**
    * @param mode see modes above
    * @param specLimit how many specializations symbol can have max
    * @return (reachableMethods, reachableTypes, casts, outerMethod)
    */
  def buildCallGraph(mode: Int, specLimit: Int)(implicit ctx: Context): (Set[CallWithContext], Set[TypeWithContext], Set[Cast], Set[Symbol]) = {
    val startTime = java.lang.System.currentTimeMillis()
    val collectedSummaries = ctx.summariesPhase.asInstanceOf[CollectSummaries].methodSummaries.map(x => (x.methodDef, x)).toMap
    val reachableMethods = new Worklist[CallWithContext]()
    val reachableTypes = new Worklist[TypeWithContext]()
    val casts = new Worklist[Cast]()
    val outerMethod = mutable.Set[Symbol]()
    val typesByMemberNameCache = new java.util.IdentityHashMap[Name, Set[TypeWithContext]]()

    def getTypesByMemberName(x: Name): Set[TypeWithContext] = {
      val ret1 = typesByMemberNameCache.get(x)
      if (ret1 eq null) {
        // not yet computed
        val upd = reachableTypes.reachableItems.filter(tp => tp.tp.member(x).exists).toSet
        typesByMemberNameCache.put(x, upd)
        upd
      } else ret1
    }
    def addReachableType(x: TypeWithContext, from: CallWithContext): Unit = {
      if (!reachableTypes.reachableItems.contains(x)) {
        reachableTypes += x
        val namesInType = x.tp.memberNames(takeAllFilter).filter(typesByMemberNameCache.containsKey)
        for (name <- namesInType) {
            typesByMemberNameCache.put(name, typesByMemberNameCache.get(name) + x)
        }
      }
    }
    // val callSites = new Worklist[CallInfo]()


    def regularizeType(t: Type): Type =
      t

    def pushEntryPoint(s: Symbol) = {
      val tpe = ref(s).tpe
      val targs = tpe.widen match {
        case t: PolyType => t.paramNames.size
        case _ => 0
      }
      val call = new CallWithContext(tpe, (0 until targs).map(x => new ErazedType()).toList, ctx.definitions.ArrayOf(ctx.definitions.StringType) :: Nil, OuterTargs.empty, null, null)
      reachableMethods += call
      val t = regularizeType(ref(s.owner).tpe)
      val self = new TypeWithContext(t, parentRefinements(t))
      addReachableType(self, null)
    }

    collectedSummaries.values.foreach(x => if(isEntryPoint(x.methodDef)) pushEntryPoint(x.methodDef))
    println(s"\t Found ${reachableMethods.size} entry points")

    def registerParentModules(tp: Type, from: CallWithContext): Unit = {
      var tp1 = tp
      while ((tp1 ne NoType) && (tp1 ne NoPrefix)) {
        if (tp1.widen ne tp1) registerParentModules(tp1.widen, from)
        if (tp1.dealias ne tp1) registerParentModules(tp1.dealias, from)
        if (tp1.termSymbol.is(Flags.Module)) {
          // reachableTypes += regularizeType(ref(tp1.termSymbol).tpe)
        } else if (tp1.typeSymbol.is(Flags.Module, Flags.Package)) {
          val t = regularizeType(ref(tp1.typeSymbol).tpe)
          addReachableType(new TypeWithContext(t, parentRefinements(t)), from)
        }
        tp1 = tp1.normalizedPrefix
      }
    }

    def instantiateCallSite(caller: CallWithContext, rec: Type, callee: CallInfo, types: Traversable[TypeWithContext]): Traversable[CallWithContext] = {

      val receiver = callee.call.normalizedPrefix
      registerParentModules(receiver, caller)

      val calleeSymbol = callee.call.termSymbol.asTerm
      val callerSymbol = caller.call.termSymbol

      val tpamsOuter = caller.call.widen match {
        case t: PolyType =>
          (t.paramNames zip caller.targs).foldLeft(OuterTargs.empty)((x, nameType) => x.+(callerSymbol, nameType._1, nameType._2))
        case _ =>
          OuterTargs.empty
      }

      lazy val outerParent = if (callerSymbol.owner ne caller.call.normalizedPrefix.classSymbol) {
        val current = caller.call.normalizedPrefix
        val superTpe = callerSymbol.owner.info
        val outers = current.typeMembers.foldLeft(OuterTargs.empty) { (outerTargs: OuterTargs, x) =>
          val old = superTpe.member(x.symbol.name)
          if (old.exists) outerTargs + (callerSymbol.owner, x.symbol.name, x.info) else outerTargs
        }
        outers
      } else OuterTargs.empty

      lazy val outerPropagetedTargs = caller.outerTargs ++ tpamsOuter ++ outerParent
      lazy val substitution = new SubstituteByParentMap(outerPropagetedTargs)

      def propagateArgs(tp: Type): Type = {
        tp match {
          case x: TermRef if mode >= AnalyseArgs && x.symbol.is(Param) && x.symbol.owner == caller.call.termSymbol =>
            val id = caller.call.termSymbol.info.paramNamess.flatten.indexWhere(_ == x.symbol.name)
            caller.argumentsPassed(id)
          case t => t
        }
      }

      def propagateTargs(tp0: Type, isConstructor: Boolean = false): Type = {
        val tp = propagateArgs(tp0)
        if (mode >= AnalyseTypes && (caller.targs.nonEmpty || caller.outerTargs.nonEmpty || (callerSymbol.owner ne caller.call.normalizedPrefix.classSymbol))) {
          /* && tp.widenDealias.existsPart{x => val t = x.typeSymbol; t.exists && (t.owner == callerSymbol || caller.outerTargs.contains(t.owner))}*/

          val refinedClassType = if (isConstructor) {
            val refinedConstructedType = tp.typeMembers.foldLeft(tp){(acc, memberType) =>
              val refinedInfo = substitution.apply(memberType.info)
              if (refinedInfo ne memberType.info) RefinedType(acc, memberType.symbol.name, refinedInfo)
              else acc
            }
            refinedConstructedType
          } else if (mode >= AnalyseArgs && (tp.isInstanceOf[PreciseType] || tp.isInstanceOf[ClosureType])) tp
            else tp.widenDealias
          val r = substitution.apply(refinedClassType)
          // for simplification only
          if (r =:= tp) tp
          else r
        } else tp
      }

      val outerTargs: OuterTargs =
        if (mode < AnalyseTypes) OuterTargs.empty
        else if (calleeSymbol.isProperlyContainedIn(callerSymbol)) {
          parentRefinements(propagateTargs(receiver)) ++ caller.outerTargs ++ tpamsOuter
        } else {
          parentRefinements(propagateTargs(receiver)) ++ new OuterTargs(caller.outerTargs.mp.filter(x => calleeSymbol.isProperlyContainedIn(x._1)))
          // todo: Is AsSeenFrom ever needed for outerTags?
        }

      // if typearg of callee is a typeparam of caller, propagate typearg from caller to callee
      lazy val targs = callee.targs map {
        case t: TypeVar if mode >= AnalyseTypes && t.stripTypeVar.typeSymbol.maybeOwner == caller.call.termSymbol =>
          assert(caller.call.termSymbol.exists)
          val abstractSym = t.stripTypeVar.typeSymbol
          val id = caller.call.termSymbol.info.asInstanceOf[PolyType].paramNames.indexOf(abstractSym.name)
          propagateTargs(caller.targs(id).stripTypeVar)
        case t if mode >= AnalyseTypes=> propagateTargs(t.stripTypeVar)
        case t => t.stripTypeVar
      }
      // if arg of callee is a param of caller, propagate arg fro caller to callee
      val args = callee.argumentsPassed.map {
        case x if x.isRepeatedParam =>
          val t = regularizeType(propagateTargs(x.translateParameterized(defn.RepeatedParamClass, ctx.requiredClass("scala.collection.mutable.WrappedArray"))))
          addReachableType(new TypeWithContext(t, parentRefinements(t)), caller)
          t
        case x if mode < AnalyseArgs =>
          ref(Summaries.simplifiedClassOf(x)).tpe
        case x: PreciseType =>
          x
        case x: ClosureType =>
          val utpe =  regularizeType(propagateTargs(x.underlying, isConstructor = true))
          val outer = parentRefinements(utpe) ++ outerTargs
          val closureT = new ClosureType(x.meth, utpe, x.implementedMethod, outer)
          addReachableType(new TypeWithContext(closureT, outer), caller)
          closureT
        case x: TermRef if x.symbol.is(Param) && x.symbol.owner == caller.call.termSymbol =>  // todo: we could also handle outer arguments
          val id = caller.call.termSymbol.info.paramNamess.flatten.indexWhere(_ == x.symbol.name)
          caller.argumentsPassed(id)
        case x => propagateTargs(x)
      }

      def addCast(from: Type, to: Type) =
        if (!(from <:< to) && to.classSymbols.forall(!_.derivesFrom(defn.NothingClass))) {
          val newCast = new Cast(from, to)

          for (tp <- reachableTypes.reachableItems) {
            if (from.classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x))) && to.classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x)))) {
              casts += newCast
              tp.castsCache += newCast
            }
          }
        }

      def filterTypes(tp1: Type, tp2: Type): Boolean = {
        if (mode >= AnalyseTypes) tp1 <:< tp2
        else {
          val tp1w = tp1.widenDealias
          val tp2w = tp2.widenDealias
          val tp2c = tp2w.classSymbol.orElse(TypeErasure.erasure(tp2).classSymbol)
          tp1w.derivesFrom(tp2c)
        }
      }
      def dispatchCalls(recieverType: Type): Traversable[CallWithContext] = {
        recieverType match {
          case t: PreciseType =>
            new CallWithContext(t.underlying.select(calleeSymbol.name), targs, args, outerTargs, caller, callee) :: Nil
          case t: ClosureType if (calleeSymbol.name eq t.implementedMethod.name) =>
            val methodSym = t.meth.meth.symbol.asTerm
            new CallWithContext(TermRef.withFixedSym(t.underlying, methodSym.name,  methodSym), targs, t.meth.env.map(_.tpe) ++ args, outerTargs ++ t.outerTargs, caller, callee) :: Nil
          case _ =>
            // without casts
            val dirrect =
              for (tp <- getTypesByMemberName(calleeSymbol.name)
                   if filterTypes(tp.tp, recieverType.widenDealias);
                   alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.asSeenFrom(tp.tp).matches(calleeSymbol.asSeenFrom(tp.tp)))
                   if alt.exists
              )
                yield new CallWithContext(tp.tp.select(alt.symbol), targs, args, outerTargs ++ tp.outerTargs, caller, callee)

            val casted = if (mode < AnalyseTypes) Nil
            else
              for (tp <- getTypesByMemberName(calleeSymbol.name);
                   cast <- tp.castsCache
                   if /*filterTypes(tp.tp, cast.from) &&*/ filterTypes(cast.to, recieverType) && {
                     val receiverBases = recieverType.classSymbols
                     val targetBases = cast.to.classSymbols
                     receiverBases.forall(c => targetBases.exists(_.derivesFrom(c)))
                     //cast.to.classSymbol != defn.NothingClass
                   };
                   alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.matches(calleeSymbol.asSeenFrom(tp.tp)))
                   if alt.exists && {
                     // this additionaly introduces a cast of result type and argument types

                     val uncastedSig = tp.tp.select(alt.symbol).widen.appliedTo(targs).widen
                     val castedSig = recieverType.select(calleeSymbol).widen.appliedTo(targs).widen
                     (uncastedSig.paramTypess.flatten zip castedSig.paramTypess.flatten) foreach (x => addCast(x._2, x._1))
                     addCast(uncastedSig.finalResultType, castedSig.finalResultType)

                     true
                   })
                yield new CallWithContext(tp.tp.select(alt.symbol), targs, args, outerTargs ++ tp.outerTargs, caller, callee)

            dirrect ++ casted
        }
      }

      receiver match {
        case _ if calleeSymbol == ctx.definitions.throwMethod =>
          Nil
        case _ if calleeSymbol == ctx.definitions.Any_asInstanceOf =>
          val from = propagateTargs(receiver)
          val to = propagateTargs(targs.head)
          addCast(from, to)
          Nil
          // TODO: handle == and !=
        case _ if defn.ObjectMethods.contains(calleeSymbol) || defn.AnyMethods.contains(calleeSymbol) =>
          // TODO: only for paper
          Nil
        case NoPrefix =>  // inner method
          assert(callee.call.termSymbol.owner.is(Method) || callee.call.termSymbol.owner.isLocalDummy)
          new CallWithContext(TermRef.withFixedSym(caller.call.normalizedPrefix, calleeSymbol.name, calleeSymbol), targs, args, outerTargs, caller, callee) :: Nil

        case t if calleeSymbol.isPrimaryConstructor =>

          val constructedType = callee.call.widen.appliedTo(targs).widen.resultType
          val fixNoPrefix = if (constructedType.normalizedPrefix eq NoPrefix) {
            var currentPrefix = caller.call.normalizedPrefix
            while (!currentPrefix.classSymbol.exists) {
              currentPrefix = currentPrefix.normalizedPrefix
              currentPrefix = currentPrefix match {
                case t: ThisType =>
                  t.tref

                case _ => currentPrefix
              }
            }
            constructedType match {
              case constructedType @ TypeRef(prefix, name)  =>
                constructedType.underlying match {
                  case ci: ClassInfo =>
                    val nci = ci.derivedClassInfo(prefix = currentPrefix) // todo: do this only for inner anonym classes
                    TypeRef.withFixedSymAndInfo(currentPrefix, name, constructedType.symbol.asType, nci)
                }
            }
          } else constructedType

          val tpe =  regularizeType(propagateTargs(fixNoPrefix, isConstructor = true))
          addReachableType(new TypeWithContext(tpe, parentRefinements(tpe) ++ outerTargs), caller)

          new CallWithContext(propagateTargs(receiver).select(calleeSymbol), targs, args, outerTargs, caller, callee) :: Nil

          // super call in a class (know target precisely)
        case st: SuperType =>
          val thisTpe = st.thistpe
          val superTpe = st.supertpe
          val targetClass = st.supertpe.baseClasses.find(clz =>
            clz.info.decl(calleeSymbol.name).altsWith(p => p.signature == calleeSymbol.signature).nonEmpty
          )
          val targetMethod = targetClass.get.info.member(calleeSymbol.name).altsWith(p => p.signature == calleeSymbol.signature).head.symbol.asTerm
          val thisTpePropagated = propagateTargs(thisTpe)


          new CallWithContext(TermRef.withFixedSym(thisTpePropagated, targetMethod.name, targetMethod), targs, args, outerTargs, caller, callee) :: Nil

          // super call in a trait
        case t if calleeSymbol.is(Flags.SuperAccessor) =>

          // Taken from ResolveSuper.rebindSuper
          val unexpandedAccName =
            if (calleeSymbol.is(ExpandedName))  // Cannot use unexpandedName because of #765. t2183.scala would fail if we did.
              calleeSymbol.name
                .drop(calleeSymbol.name.indexOfSlice(nme.EXPAND_SEPARATOR ++ nme.SUPER_PREFIX))
                .drop(nme.EXPAND_SEPARATOR.length)
            else calleeSymbol.name

          val SuperAccessorName(memberName) = unexpandedAccName: Name

          val prev = t.widenDealias.classSymbol
          getTypesByMemberName(memberName).flatMap {
            x =>
              val s = x.tp.baseClasses.dropWhile(_ != prev)
              if (s.nonEmpty) {
                val parentMethod = ResolveSuper.rebindSuper(x.tp.widenDealias.classSymbol, calleeSymbol).asTerm
                // todo: outerTargs are here defined in terms of location of the subclass. Is this correct?
                new CallWithContext(TermRef.withFixedSym(t, parentMethod.name, parentMethod), targs, args, outerTargs, caller, callee) :: Nil

              } else Nil
          }

        case thisType: ThisType if (!(calleeSymbol.owner.flags is PackageCreationFlags)) =>
          val dropUntil = thisType.tref.classSymbol
          var currentThis = caller.call.normalizedPrefix
          var currentOwner = caller.call.termSymbol.owner
          while ((currentOwner ne dropUntil) && (currentThis ne NoType)) {
            currentThis = currentThis.normalizedPrefix
            currentOwner = currentOwner.owner.enclosingClass
          }
          if (currentThis.derivesFrom(thisType.cls)) {
            val fullThisType = AndType.apply(currentThis, thisType.tref)
            if (calleeSymbol.is(Private))
              new CallWithContext(TermRef.withFixedSym(currentThis, calleeSymbol.name, calleeSymbol), targs, args, outerTargs, caller, callee) :: Nil
            else dispatchCalls(propagateTargs(fullThisType))
          } else {
            dispatchCalls(propagateTargs(receiver.widenDealias))
          }

          // todo: handle calls on this of outer classes


        case _: PreciseType =>
          dispatchCalls(propagateTargs(receiver))
        case _: ClosureType =>
          dispatchCalls(propagateTargs(receiver))
        case x: TermRef if x.symbol.is(Param) && x.symbol.owner == caller.call.termSymbol =>
          dispatchCalls(propagateTargs(receiver))
        case _ =>
          dispatchCalls(propagateTargs(receiver.widenDealias))
      }
    }

    def processCallSites(callSites: immutable.Set[CallWithContext], instantiatedTypes: immutable.Set[TypeWithContext]): Unit = {

      for (method <- callSites) {
        // Find new call sites

        val sym = method.call.normalizedPrefix match {
          case t: ClosureType =>
            t.meth.meth.symbol
          case _ =>
            method.call.termSymbol
        }

        reachableMethods ++= {
          val summary = collectedSummaries.get(sym)

          if (summary.isDefined) {

            summary.get.accessedModules.map(x => new TypeWithContext(regularizeType(x.info), parentRefinements(x.info))).foreach(x => addReachableType(x, method))
            // 296 and 298 are the same (?)
            summary.get.methodsCalled.flatMap { x =>
              val reciever = x._1
              x._2.flatMap{callSite =>
                val insts = instantiateCallSite(method, reciever, callSite, instantiatedTypes)
                val nw = insts.filter(x => !method.outEdges(callSite).contains(x))
                method.outEdges(callSite) = nw.toList ::: method.outEdges(callSite)
                insts // infinite cycle
                // nw // works
              }
            }
          } else {
            outerMethod += sym
            Nil
          }
        }

      }

    }


    while(reachableMethods.nonEmpty || reachableTypes.nonEmpty || casts.nonEmpty) {
      reachableTypes.clear
      casts.clear
      reachableMethods.clear

      processCallSites(reachableMethods.reachableItems.toSet, reachableTypes.reachableItems.toSet)

      println(s"\t Found ${reachableTypes.size} new instantiated types")
      val newReachableTypes = reachableTypes.newItems
      newReachableTypes.foreach { x =>
        val clas = x.tp match {
          case t: ClosureType =>
            t.u.classSymbol.asClass
          case _ => x.tp.classSymbol.asClass
        }
        if (!clas.is(Flags.JavaDefined) && clas.is(Flags.Module)) {
          val fields = clas.classInfo.decls.filter(x => !x.is(Flags.Method) && !x.isType)
          val parent = new CallWithContext(x.tp.select(clas.primaryConstructor), x.tp.baseArgInfos(clas), Nil, x.outerTargs, null, null)
          reachableMethods ++= fields.map {
            fieldSym =>
              new CallWithContext(x.tp.select(fieldSym), Nil, Nil, x.outerTargs, parent, null)
          }
        }
      }

      println(s"\t Found ${reachableMethods.size} new call sites: ${reachableMethods.newItems.toString().take(60)}")

    }

    val endTime = java.lang.System.currentTimeMillis()
    println("++++++++++ finished in " + (endTime - startTime)/1000.0  +" seconds. ++++++++++ ")
    (reachableMethods.reachableItems.toSet, reachableTypes.reachableItems.toSet, casts.reachableItems.toSet, outerMethod.toSet)
  }

  def outputGraph(mode: Int, specLimit: Int)(reachableMethods: Set[CallWithContext],
                  reachableTypes: Set[TypeWithContext],
                  casts: Set[Cast],
                  outerMethod: Set[Symbol])(implicit ctx: Context): String = {
    val classesWithReachableMethods = reachableMethods.map( _.call.termSymbol.maybeOwner.info.widen.classSymbol)
    val reachableClasses = classesWithReachableMethods ++ reachableTypes.flatMap(x => x.tp.classSymbols).flatMap(_.baseClasses)
    val reachableDefs = reachableMethods.map(_.call.termSymbol)

    /*val filter = scala.io.Source.fromFile("trace-filtered").getLines().toList
    /val filterUnMangled = filter.map(x => x.replace("::", ".").replace("$class", "")).toSet

    def fil(x: Symbol) =
      filterUnMangled.contains(x.fullName.toString)

    val liveDefs = reachableDefs.filter{x => fil(x)}     */


    val reachableSpecs: Set[(Symbol, List[Type])] = reachableMethods.flatMap { x =>
      val clas = x.call.termSymbol.maybeOwner.info.widen.classSymbol
      val meth = x.call.termSymbol
      if (mode >= AnalyseTypes) (meth, x.call.normalizedPrefix.baseArgInfos(clas)) :: Nil
      else {
        val clazSpecializationsCount =
          if (clas.primaryConstructor.info.widenDealias.isInstanceOf[PolyType]) specLimit
          else 1
        val methodSpecializationsCount =
          if (meth.info.widenDealias.isInstanceOf[PolyType]) specLimit
          else 1
        println(s"specializing $clas $meth for $clazSpecializationsCount * $methodSpecializationsCount")
        (0 until clazSpecializationsCount*methodSpecializationsCount).map(x => (meth, ConstantType(Constant(x)):: Nil)).toList

      }
    }

    val morphisms = reachableMethods.groupBy(x => x.callee).groupBy(x => x._2.map(_.call.termSymbol).toSet.size)

    val mono = if(morphisms.contains(1)) morphisms(1) else Map.empty
    val bi = if(morphisms.contains(2)) morphisms(2) else Map.empty
    val mega = morphisms - 1 - 2

    println(s"\t Found: ${classesWithReachableMethods.size} classes with reachable methods, ${reachableClasses.size} reachable classes, ${reachableDefs.size} reachable methods, ${reachableSpecs.size} specializations")
    println(s"\t mono: ${mono.size}, bi: ${bi.size}, mega: ${mega.map(_._2.size).sum}")
    println(s"\t Found ${outerMethod.size} not defined calls: ${outerMethod.map(_.showFullName)}")
    println(s"\t Reachable classes: ${classesWithReachableMethods.mkString(", ")}")
    println(s"\t Reachable methods: ${reachableDefs.mkString(", ")}")
    println(s"\t Reachable specs: ${reachableSpecs.mkString(", ")}")
    println(s"\t Primary Constructor specs: ${reachableSpecs.filter(_._1.isPrimaryConstructor).map(x => (x._1.showFullName, x._2))}")

    val outGraph = new StringBuffer()
    outGraph.append(s"digraph Gr${mode}_$specLimit {\n")
    outGraph.append("graph [fontsize=10 fontname=\"Verdana\" compound=true];\n")
    outGraph.append("label = \""+reachableMethods.size + " nodes, "
      + reachableMethods.foldLeft(0)(_ + _.outEdges.values.foldLeft(0)(_ + _.size)) +" edges, "+ reachableTypes.size  +" reachable types\";\n")

    val slash = '"'

    def escape(s: String) = s.replace("\\", "\\\\").replace("\"","\\\"")

    def fullNameSeparated(symbol: Symbol)(separator: String)(implicit ctx: Context): Name = {
      var sep = separator
      val owner = symbol.owner
      var name: Name = symbol.name
      var stopAtPackage = false
      if (sep.isEmpty) {
        sep = "$"
        stopAtPackage = true
      }
      if (symbol.isAnonymousClass || symbol.isAnonymousFunction)
         name = name ++ symbol.id.toString
      if (symbol == NoSymbol ||
        owner == NoSymbol ||
        owner.isEffectiveRoot ||
        stopAtPackage && owner.is(PackageClass)) name
      else {
        var encl = owner
        while (!encl.isClass && !encl.isPackageObject) {
          encl = encl.owner
          sep += "~"
        }
        if (owner.is(ModuleClass, butNot = Package) && sep == "$") sep = "" // duplicate scalac's behavior: don't write a double '$$' for module class members.
        val fn = fullNameSeparated(encl)(separator) ++ sep ++ name
        if (symbol.isType) fn.toTypeName else fn.toTermName
      }
    }

    def symbolName(s: Symbol): String = {
      if (!s.is(Flags.Method))
        escape(s.name.show)
      else
        escape(fullNameSeparated(s)(".").show)
    }

    def typeName(x: Type): String = {
      x match {
        case ConstantType(value) => s"${escape(value.toString)}"
        case _ =>
          val t = x.termSymbol.orElse(x.typeSymbol)
          if (t.exists)
            symbolName(t)
          else escape(x.show)
      }
    }

    def csWTToName(x: CallWithContext, close: Boolean = true, open: Boolean = true): String = {
      if (x.call.termSymbol.owner == x.call.normalizedPrefix.classSymbol) {
        s"${if (open) slash else ""}${typeName(x.call)}${if (x.targs.nonEmpty) "[" + x.targs.map(x => typeName(x)).mkString(",") + "]" else ""}${if (close) slash else ""}"
      } else {
        s"${if (open) slash else ""}${typeName(x.call.normalizedPrefix)}.super.${symbolName(x.call.termSymbol)}${if (x.targs.nonEmpty) "[" + x.targs.map(x => typeName(x)).mkString(",") + "]" else ""}${if (close) slash else ""}"
      }
    }

    def csToName(parrent: CallWithContext, inner: CallInfo): String = {
      csWTToName(parrent, close = false) + s"${escape(inner.call.show)}${inner.hashCode()}$slash"
    }

    def dummyName(x: CallWithContext) = {
      csWTToName(x, close = false) + "_Dummy\""
    }

    def clusterName(x: CallWithContext) = {
      val r =  "\"cluster_" + csWTToName(x, open = false)
//      if (r.contains("BufferLike.apply")) {
//        println("doba")
//      }
      r
    }

    // add names and subraphs
    reachableMethods.foreach { caller =>
      def callSiteLabel(x: CallInfo): String = {
        val prefix = x.call.normalizedPrefix
        val calleeSymbol = x.call.termSymbol
        prefix match {
          case NoPrefix => calleeSymbol.name.toString
          case t if calleeSymbol.isPrimaryConstructor => calleeSymbol.showFullName
          case st: SuperType => s"super[${st.supertpe.classSymbol.showFullName}].${calleeSymbol.name}"
          /* case t if calleeSymbol.is(Flags.SuperAccessor) =>
             val prev = t.classSymbol
             types.flatMap {
               x =>
                 val s = x.baseClasses.dropWhile(_ != prev)
                 if (s.nonEmpty) {
                   val parent = s.find(x => x.info.decl(calleeSymbol.name).altsWith(x => x.signature == calleeSymbol.signature).nonEmpty)
                   parent match {
                     case Some(p) if p.exists =>
                       val method = p.info.decl(calleeSymbol.name).altsWith(x => x.signature == calleeSymbol.signature)
                       // todo: outerTargs are here defined in terms of location of the subclass. Is this correct?
                       new CallWithContext(t.select(method.head.symbol), targs, args, outerTargs) :: Nil
                     case _ => Nil
                   }
                 } else Nil
             }     */

          case thisType: ThisType => s"this.${calleeSymbol.name}"
          case t =>
            s"${typeName(t)}.${calleeSymbol.name}"
        }
      }

      val line = s"""subgraph ${clusterName(caller)} {
          label = ${csWTToName(caller)}; color = ${if (outerMethod.contains(caller.call.termSymbol)) "red" else "blue"};
          ${dummyName(caller)} [shape=point style=invis];
        ${caller.outEdges.keys.map(x => csToName(caller, x) + s" [label = $slash${callSiteLabel(x)}$slash];").mkString("\n")}
        }
        """
      outGraph.append(line)
      val edges = caller.outEdges.foreach { x =>
        val callInfo = x._1
        x._2.foreach { target =>
          val line = s"${csToName(caller, callInfo)} -> ${dummyName(target)} [ltail=${clusterName(target)}];\n"
          outGraph.append(line)
        }
      }
    }
    outGraph.append("}")
    outGraph.toString
  }

  def sendSpecializationRequests(reachableMethods: Set[CallWithContext],
                                  reachableTypes: Set[TypeWithContext],
                                  casts: Set[Cast],
                                  outerMethod: Set[Symbol])(implicit ctx: Context): Unit = {
//   ctx.outerSpecPhase match {
//      case specPhase: OuterSpecializer =>
//
//        reachableMethods.foreach { mc =>
//          val methodSym = mc.call.termSymbol
//          val outerTargs = methodSym.info.widen match {
//            case PolyType(names) =>
//              (names zip mc.targs).foldLeft(mc.outerTargs)((x, nameType) => x.+(methodSym, nameType._1, nameType._2))
//            case _ =>
//              mc.outerTargs
//          }
//          if (outerTargs.mp.nonEmpty && !methodSym.isPrimaryConstructor)
//            specPhase.registerSpecializationRequest(methodSym)(outerTargs)
//        }
//        reachableTypes.foreach { tpc =>
//          val parentOverrides = tpc.tp.typeMembers(ctx).foldLeft(OuterTargs.empty)((outerTargs, denot) =>
//            denot.symbol.allOverriddenSymbols.foldLeft(outerTargs)((outerTargs, sym) =>
//              outerTargs.+(sym.owner, denot.symbol.name, denot.info)))
//
//          val spec = tpc.outerTargs ++ parentOverrides ++ parentRefinements(tpc.tp)
//
//          if (spec.nonEmpty) {
//            specPhase.registerSpecializationRequest(tpc.tp.typeSymbol)(spec)
//            def loop(remaining: List[Symbol]): Unit = {
//              if (remaining.isEmpty) return;
//              val target = remaining.head
//
//              val nspec = OuterTargs(spec.mp.filter{x => target.derivesFrom(x._1)})
//              if (nspec.nonEmpty)
//                specPhase.registerSpecializationRequest(target)(nspec)
//              loop(remaining.tail)
//            }
//            val parents = tpc.tp.baseClasses
//            loop(parents)
//          }
//        }
//      case _ =>
//       ctx.warning("No specializer phase found")
//    }

  }

  var runOnce = true
  def run(implicit ctx: Context): Unit = {
    if (runOnce /*&& ctx.settings.lto.value.nonEmpty*/) {
      val specLimit = 15
      //println(s"\n\t\t\tOriginal analisys")
      //val g1 = buildCallGraph(AnalyseOrig, specLimit)

      //println(s"\n\t\t\tType flow analisys")
      //val g2 = buildCallGraph(AnalyseTypes, specLimit)

      println(s"\n\t\t\tType & Arg flow analisys")
      val cg = buildCallGraph(AnalyseArgs, specLimit)
      reachableMethods = cg._1; reachableTypes = cg._2; casts = cg._3; outerMethods = cg._4
      val g3 = outputGraph(AnalyseArgs, specLimit)(reachableMethods, reachableTypes, casts, outerMethods)
      sendSpecializationRequests(reachableMethods, reachableTypes, casts, outerMethods)

      def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
        val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
      }

      //printToFile(new java.io.File("out1.dot")) { out =>
      //  out.println(g1)
      //}
     // printToFile(new java.io.File("out2.dot")) { out =>
     //   out.println(g2)
     // }
      printToFile(new java.io.File("out3.dot")) { out =>
        out.println(g3)
      }

    }
    runOnce = false
  }
}

object CollectSummaries {

  class SubstituteByParentMap(substMap: OuterTargs)(implicit ctx: Context) extends DeepTypeMap()(ctx) {
    def apply(tp: Type): Type = {
      lazy val substitution = substMap.mp.getOrElse(tp.typeSymbol.owner, Nil)
      def termTypeIfNeed(t: Type): Type = {
        if (tp.isInstanceOf[TermType] && !t.isInstanceOf[TermType]) {
          t match {
            case t: TypeAlias =>
              assert(t.underlying.isInstanceOf[TermType])
              t.underlying
            case t: ClassInfo =>
              t.typeRef
            case _ =>
              assert(false)
              ???
          }
        } else t
      }
      tp match {
        case tp: RefinedType => mapOver(tp) // otherwise we will loose refinement
        case tp: TypeAlias => mapOver(tp) // map underlying
        case tp if tp.typeSymbol.exists && substitution.nonEmpty =>
          var typ = tp
          /*val id = tp.typeSymbol.owner.info match {
            case t: PolyType =>
              t.paramNames.indexOf(tp.typeSymbol.name)
            case t: ClassInfo =>
              var typ = tp
              var id = t.typeParams.indexOf(typ.typeSymbol)
              while (id < 0 && (tp.typeSymbol.info.typeSymbol ne tp.typeSymbol)) {
                typ = tp.typeSymbol.info
                id = t.typeParams.indexOf(typ.typeSymbol)
              }
              id
            case _ =>
              -2
          } */
          var id = substitution.find(x => x._1 == tp.typeSymbol.name)
          var limit = 30
          var stack: List[Type] = Nil
          while (id.isEmpty && (limit > 0) && (typ.typeSymbol.info.typeSymbol ne typ.typeSymbol)) {
            typ = typ.typeSymbol.info
            stack = typ :: stack
            id = substitution.find(x => x._1 == typ.typeSymbol.name)
            limit -= 1
          }

          // assert(id.isDefined)
          if (id.isDefined) {
            val t = termTypeIfNeed(id.get._2.stripTypeVar)
            if (!(t =:= typ))
             apply(termTypeIfNeed(t))
            else t
          } else tp
        case t: TypeRef if (t.prefix.normalizedPrefix eq NoPrefix) =>
          val tmp = apply(t.info)
          if (tmp ne t.info) termTypeIfNeed(tmp)
          else mapOver(t)
        case _ => mapOver(tp)

      }

    }
  }

  def substName = "substituted".toTypeName

  private val forgetHistory = false

}
