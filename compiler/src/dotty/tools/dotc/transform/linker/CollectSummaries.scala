package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.FromTasty.TASTYCompilationUnit
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeTransforms._

import dotty.tools.dotc.transform.linker.Summaries.OuterTargs

import scala.annotation.tailrec
import scala.collection.mutable

class CollectSummaries extends MiniPhase { thisTransform =>
  import tpd._
  import Summaries._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "summaries"

  val treeTransform: TreeTransform = new Collect

  private var methodSums = List[MethodSummary]()
  // private var noSummaryAvailable = Set[Symbol]()

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

    var methodSummaries = List[MethodSummary]()
    var methodSummaryStack = mutable.Stack[MethodSummary]()
    var curMethodSummary: MethodSummary = _

    override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
      if (ctx.compilationUnit.isInstanceOf[TASTYCompilationUnit])
        NoTransform // will retrieve them lazily
      else this
    }

    override def prepareForDefDef(tree: tpd.DefDef)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if (!sym.is(Label) && !sym.isPrimaryConstructor) {
        methodSummaryStack.push(curMethodSummary)
        val args = tree.vparamss.flatten.map(_.symbol) // outer param for constructors
        val argumentStoredToHeap = (0 to args.length).map(_ => true).toList
        curMethodSummary = MethodSummary(sym, thisAccessed = false, mutable.Map.empty, Nil, -1, argumentStoredToHeap)
      }
      if (sym.isPrimaryConstructor) {
        sym.owner.mixins.foreach { mixin =>
          val decl = mixin.primaryConstructor
          val initRef = ref(NamedType(sym.owner.typeRef, decl.name, decl.denot))
          decl.info match {
            case tp: PolyType =>
              if (tp.resType.paramTypess.iterator.flatten.isEmpty)
                registerCall(Apply(TypeApply(initRef, tp.paramRefs.map(TypeTree(_))), Nil)) // TODO get precise type params
            case tp =>
              if (tp.paramTypess.iterator.flatten.isEmpty)
                registerCall(Apply(initRef, Nil))
          }
        }
      }
      this
    }

    override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Label) && !tree.symbol.isPrimaryConstructor) {
        assert(curMethodSummary.methodDef eq tree.symbol)
        methodSummaries = curMethodSummary :: methodSummaries
        curMethodSummary = methodSummaryStack.pop()
      }
      tree
    }

    override def prepareForValDef(tree: tpd.ValDef)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if (sym.exists && ((sym.is(Lazy) &&  (sym.owner.is(Package) || sym.owner.isClass)) ||  //lazy vals and modules
          sym.owner.name.startsWith(nme.LOCALDUMMY_PREFIX) || // blocks inside constructor
          sym.owner.isClass)) { // fields
        // owner is a template
        methodSummaryStack.push(curMethodSummary)
        curMethodSummary = MethodSummary(sym, thisAccessed = false, mutable.Map.empty, Nil, -1, List(true))
      }
      this
    }

    override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if (sym.exists) {
        val ownerIsClass = sym.owner.isClass
        val isLazyValOrModule = sym.is(Lazy) && (ownerIsClass || sym.owner.is(Package))
        val isBockInsideConstructor = sym.owner.name.startsWith(nme.LOCALDUMMY_PREFIX)
        if (isLazyValOrModule || isBockInsideConstructor || ownerIsClass) {
          assert(curMethodSummary.methodDef eq tree.symbol)
          methodSummaries = curMethodSummary :: methodSummaries
          curMethodSummary = methodSummaryStack.pop()
        }
        if (!isLazyValOrModule && (isBockInsideConstructor || ownerIsClass))
          registerCall(tree)
      }
      tree
    }

    override def prepareForTemplate(tree: tpd.Template)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      assert(!sym.is(Label))
      methodSummaryStack.push(curMethodSummary)
      curMethodSummary = MethodSummary(sym.owner.primaryConstructor, thisAccessed = false, mutable.Map.empty, Nil, -1, List(true))
      this
    }

    override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      assert(!sym.is(Label))
      assert(curMethodSummary.methodDef eq tree.symbol.owner.primaryConstructor)
      methodSummaries = curMethodSummary :: methodSummaries
      curMethodSummary = methodSummaryStack.pop()
      tree
    }

    /*
    override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if (sym.isClass) {
        val isEntryPoint = dotty.tools.backend.jvm.CollectEntryPoints.isJavaEntryPoint(sym)
        /*summaries = ClassSummary(sym.asClass,
          methodSummaries
        ) :: summaries
        methodSummaries = Nil*/
      }
      tree
    }
    */

    def registerModule(sym: Symbol)(implicit ctx: Context): Unit = {
      if ((curMethodSummary ne null) && sym.is(ModuleVal)) {
        curMethodSummary.accessedModules = sym :: curMethodSummary.accessedModules
        registerModule(sym.owner)
      }
      val res = sym.info.finalResultType.termSymbol
      if ((curMethodSummary ne null) && res.is(ModuleVal)) {
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

      @tailrec def receiverArgumentsAndSymbol(t: Tree, accArgs: List[List[Tree]] = Nil, accT: List[Tree] = Nil):
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
      if (!widenedTp.isInstanceOf[MethodicType] || (tree.symbol.exists && !tree.symbol.info.isInstanceOf[MethodicType])) {
        val (receiver, _ /*call*/ , arguments, typeArguments, method) = receiverArgumentsAndSymbol(tree)

        val storedReceiver = receiver.tpe

        assert(storedReceiver.exists)

        @tailrec def skipBlocks(s: Tree): Tree = {
          s match {
            case s: Block => skipBlocks(s.expr)
            case _ => s
          }
        }

        @tailrec def argType(x: Tree): Type = skipBlocks(x) match {
          case exp: Closure =>
            val SAMType(e) = exp.tpe
            new ClosureType(exp, x.tpe, e.symbol, null.asInstanceOf[OuterTargs])
          case Select(New(tp), _) => new PreciseType(tp.tpe)
          case Apply(Select(New(tp), _), args) => new PreciseType(tp.tpe)
          case Apply(TypeApply(Select(New(tp), _), targs), args) => new PreciseType(tp.tpe)
          case Typed(expr, _) => argType(expr)
          case _ =>
            x.tpe match {
              case _ if x.isInstanceOf[NamedArg] => ref(symbolOf(x.asInstanceOf[NamedArg].arg)).tpe
              case _ => x.tpe
            }
        }

        val thisCallInfo = CallInfo(method, typeArguments.map(_.tpe), arguments.flatten.map(argType))

        // Create calls to wrapXArray for varArgs
        val repeatedArgsCalls = tree match {
          case Apply(fun, _) if fun.symbol.info.isVarArgsMethod =>
            @tailrec def refine(tp: Type): Type = tp match {
              case tp: TypeAlias => refine(tp.alias.dealias)
              case tp: RefinedType => refine(tp.refinedInfo)
              case _ => tp
            }
            @tailrec def getVarArgTypes(tp: Type, acc: List[Type] = Nil): List[Type] = tp match {
              case tp: PolyType => getVarArgTypes(tp.resultType, acc)
              case tp@MethodType(_, paramTypes) if paramTypes.nonEmpty && paramTypes.last.isRepeatedParam =>
                getVarArgTypes(tp.resultType, refine(paramTypes.last) :: acc)
              case _ => acc
            }

            def wrapArrayTermRef(arrayName: TermName) =
              TermRef(defn.ScalaPredefModuleRef, defn.ScalaPredefModule.requiredMethod(arrayName))

            val wrapArrayCall = getVarArgTypes(fun.tpe.widenDealias).map { tp =>
              val args = List(defn.ArrayOf(tp))
              val sym = tp.typeSymbol
              if (defn.isPrimitiveClass(sym))
                CallInfo(wrapArrayTermRef(nme.wrapXArray(sym.name)), Nil, args, thisCallInfo)
              else if (sym == defn.ObjectClass)
                CallInfo(wrapArrayTermRef(nme.wrapRefArray), List(tp), args, thisCallInfo)
              else
                CallInfo(wrapArrayTermRef(nme.genericWrapArray), List(tp), args, thisCallInfo)
            }

            if (wrapArrayCall.isEmpty) wrapArrayCall
            else CallInfo(defn.ScalaPredefModuleRef, Nil, Nil, thisCallInfo) :: wrapArrayCall

          case _ => Nil
        }

        val fillInStackTrace = tree match {
          case Apply(Select(newThrowable, nme.CONSTRUCTOR), _) if newThrowable.tpe.derivesFrom(defn.ThrowableClass) =>
            val throwableClass = newThrowable.tpe.widenDealias.classSymbol
            val fillInStackTrace = throwableClass.requiredMethod("fillInStackTrace")
            if (fillInStackTrace.is(JavaDefined) || throwableClass != fillInStackTrace.owner) Nil
            else List(CallInfo(TermRef(newThrowable.tpe, fillInStackTrace), Nil, Nil, thisCallInfo))
          case _ => Nil
        }

        val languageDefinedCalls = repeatedArgsCalls ::: fillInStackTrace

        curMethodSummary.methodsCalled(storedReceiver) = thisCallInfo :: languageDefinedCalls ::: curMethodSummary.methodsCalled.getOrElse(storedReceiver, Nil)
      }
    }

    override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Package)) {
        registerModule(tree.symbol)
      }
      val select = tree.tpe match {
        case TermRef(prefix: TermRef, name) =>
          Some(tpd.ref(prefix).select(tree.symbol))
        case TermRef(prefix: ThisType, name) =>
          Some(tpd.This(prefix.cls).select(tree.symbol))
        case TermRef(NoPrefix, name) =>
          if (tree.symbol is Method) { // todo: this kills dotty {
            val widenedTp = tree.tpe.widen
            if (widenedTp.isInstanceOf[MethodicType] && (!tree.symbol.exists || tree.symbol.info.isInstanceOf[MethodicType]))
              return tree
            registerCall(tree)
            return tree
            // Some(This(tree.symbol.topLevelClass.asClass).select(tree.symbol)) // workaround #342 todo: remove after fixed
          }
          else None
        case _ => None
      }

      select.map(transformSelect)

      tree
    }

    override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Package | Label)) {
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
      if (!tree.symbol.is(Label))
       registerCall(tree)
      tree
    }

    override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      registerCall(tree)
      tree
    }

    override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {

      methodSums = methodSummaries

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

          val s = methodSummaries.filter(_.methodDef.topLevelClass == cls)

          // println(s)

          serializeCS(s, ctx.compilationUnit.picklers(cls))
      } */



      tree
    }
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
              null
          }
        } else t
      }
      tp match {
        case tp: RefinedType => mapOver(tp) // otherwise we will loose refinement
        case tp: TypeAlias => mapOver(tp) // map underlying
        case _ if tp.typeSymbol.exists && substitution.nonEmpty =>
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
        case t: TypeRef if t.prefix.normalizedPrefix eq NoPrefix =>
          val tmp = apply(t.info)
          if (tmp ne t.info) termTypeIfNeed(tmp)
          else mapOver(t)
        case _ => mapOver(tp)

      }

    }
  }

  def substName = "substituted".toTypeName

}
