package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.{Literal, ref}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.{Symbol, defn}
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.Types.{ExprType, MethodType, TermRef, Type}
import dotty.tools.dotc.core.tasty.DottyUnpickler.SectionTreeSectionUnpickler
import dotty.tools.dotc.core.tasty._
import dotty.tools.dotc.transform.linker.types.{ClosureType, PreciseType}

import scala.collection.mutable


class TastySummaries {
  import TastySummaries._

  private var noSummaryAvailable = Set[Symbol]()

  private var loadedMethodSummaries: Map[Symbol, MethodSummary] = Map.empty

  def apply(d: Symbol)(implicit ctx: Context): Option[MethodSummary] = {
    if (noSummaryAvailable(d)) None
    else {
      if (!loadedMethodSummaries.contains(d)) {
        val nw = retrieveSummary(d)
        nw.foreach(ms => loadedMethodSummaries = loadedMethodSummaries.updated(ms.methodDef, ms))
        if (!loadedMethodSummaries.contains(d))
          noSummaryAvailable += d
      }
      loadedMethodSummaries.get(d)
    }
  }

  private def retrieveSummary(sym: Symbol)(implicit ctx: Context): List[MethodSummary] = {
    val topDenot = sym.topLevelClass.denot.asSymDenotation
    if (topDenot.symbol == defn.ObjectClass)
      return Nil
    topDenot match {
      case clsd: ClassDenotation =>
        clsd.hack match {
          case Some(unpickler: DottyUnpickler) =>
            val tastySection = unpickler.unpickler.unpickle(new SectionTreeSectionUnpickler(unpickler, sectionName)).get
            tastySection.enterTopLevel(roots = Set.empty)
            val treeReader = tastySection.asInstanceOf[SectionTreeUnpickler].getStartReader.get

            val unp = new TastyUnpickler.SectionUnpickler[List[MethodSummary]](sectionName) {
              def unpickle(reader: TastyReader, tastyName: TastyName.Table): List[MethodSummary] = {
                def readSymbolRef = {
                  val s = treeReader.readType()
                  s.termSymbol.orElse(s.typeSymbol).orElse(s.classSymbol)
                }

                def readType = treeReader.readType()
                def readTerm = treeReader.readTpt()

                def readMS: MethodSummary = {

                  val sym = readSymbolRef
                  val methodsSz = reader.readInt()

                  val methodsCalled = new mutable.HashMap[Type, List[CallInfo]]()

                  for(_ <- 0 until methodsSz) {
                    val preciseReceiver :: _ = readCompactBits(reader, 1)
                    val receiver = if (preciseReceiver) new PreciseType(readType) else readType
                    val listSz = reader.readInt()

                    def readCallInfo: CallInfo = {
                      val t = readType
                      val targsSz = reader.readByte()
                      val targs = for(_ <- 0 until targsSz) yield readType
                      val argsSz = reader.readByte()
                      val argsFlags = readCompactBits(reader, argsFlagsSize * argsSz)
                      val argsPassed = for (argFlags <- argsFlags.sliding(argsFlagsSize, argsFlagsSize).toList) yield {
                        val precise :: closure :: _ = argFlags
                        if (precise) new PreciseType(readType)
                        else if (closure) new ClosureType(readTerm.asInstanceOf[tpd.Closure], readType, readSymbolRef)
                        else readType
                      }
                      val source = None // TODO
                      CallInfo(t.asInstanceOf[TermRef], targs.toList, argsPassed, source) // TODO no need to normalize the types
                    }

                    val calls = for(_ <- 0 until listSz) yield readCallInfo
                    methodsCalled(receiver) = calls.toList
                  }

                  val accessedModulesSz = reader.readInt()

                  val accessedModules = for (_ <- 0 until accessedModulesSz) yield readSymbolRef

                  val argumentReturned = reader.readByte()

                  val numParameters = sym.info match {
                    case tp: MethodType => tp.paramInfoss.foldLeft(0)(_ + _.size)
                    case _: ExprType => 0
                  }
                  val bitsExpected = numParameters + 2 // this and thisAccessed
                  val (thisAccessed :: argumentStoredToHeap) = readCompactBits(reader, bitsExpected)
                  val definedClosures = Nil // TODO
                  new MethodSummary(sym, thisAccessed, methodsCalled.toMap, definedClosures, accessedModules.toList, argumentReturned.toByte, argumentStoredToHeap.take(bitsExpected - 1))
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

}


object TastySummaries {

  val version = 1

  val sectionName = "MethodSummaries"

  private[TastySummaries] val argsFlagsSize = 2

  def saveInTasty(methodSummaries: List[MethodSummary])(implicit ctx: Context): Unit = {
    for (cls <- ctx.compilationUnit.picklers.keySet) {
      val pickler = ctx.compilationUnit.picklers(cls)

      if (!ctx.scala2Mode) {
        val buf = new TastyBuffer(5000)
        val treePickl = pickler.treePkl
        val anchorTree = tpd.SyntheticValDef((sectionName + pickler.uuid.toString).toTermName, Literal(Constant(sectionName)))

        treePickl.preRegister(anchorTree)
        treePickl.pickle(anchorTree :: Nil)

        pickler.newSection(sectionName, buf)
        val start = treePickl.buf.currentAddr
        buf.writeInt(version) //1

        def writeSymbolRef(sym: Symbol) = {
          treePickl.pickleType(ref(sym).tpe)
        }

        def writeTypeRef(tp: Type) = {
          treePickl.pickleType(tp)
        }

        def writeTreeRef(tp: tpd.Tree) = {
          treePickl.pickleTree(tp)
        }

        def serializeMS(ms: MethodSummary) = {
          writeSymbolRef(ms.methodDef) //26

          buf.writeInt(ms.methodsCalled.size) //29
          for ((receiver, methods) <- ms.methodsCalled) {
            compactBits(List(receiver.isInstanceOf[PreciseType]))
            receiver match {
              case receiver: PreciseType => writeTypeRef(receiver.underlying) //36
              case _ => writeTypeRef(receiver) //36
            }
            buf.writeInt(methods.size)

            def writeCallInfo(c: CallInfo): Unit = {
              writeTypeRef(c.call)
              buf.writeByte(c.targs.size)
              c.targs foreach writeTypeRef

              buf.writeByte(c.argumentsPassed.size)

              val argFlags =
                c.argumentsPassed.flatMap(arg => List(arg.isInstanceOf[PreciseType], arg.isInstanceOf[ClosureType]))
              assert(argFlags.size == argsFlagsSize * c.argumentsPassed.size)
              compactBits(argFlags).foreach(buf.writeByte)

              c.argumentsPassed.foreach {
                case arg: PreciseType => writeTypeRef(arg.underlying)
                case arg: ClosureType =>
                  writeTreeRef(arg.meth)
                  writeTypeRef(arg.u)
                  writeSymbolRef(arg.implementedMethod)
                case arg => writeTypeRef(arg)
              }
            }

            methods foreach writeCallInfo
          }

          buf.writeInt(ms.accessedModules.length)
          ms.accessedModules foreach writeSymbolRef

          buf.writeByte(ms.argumentReturned)
          compactBits(ms.thisAccessed :: ms.argumentStoredToHeap).foreach(buf.writeByte)
        }

        val methods = methodSummaries.filter(_.methodDef.topLevelClass == cls)

        buf.writeInt(methods.length) // 19

        methods foreach serializeMS

        val sz = treePickl.buf.currentAddr.index - start.index

        ctx.debuglog("new section for " + cls + " size:"
          + sz + "/" + buf.currentAddr + "increased by " + (sz + buf.length) * 1.0 / start.index)
        // note: this is huge overestimate. This section contains a lot of refferences to already existing symbols and types
        // and will be compressed during bytecode generation by TreePickler.compactify

      }

      pickler.treePkl.compactify()
    }
  }

  private def compactBits(ls: List[Boolean]): List[Int] = {
    def foldByte(bt: List[Boolean]) = bt.foldRight(0)((bl: Boolean, acc: Int) => (if (bl) 1 else 0) + (acc << 1))
    ls.grouped(8).map(foldByte).toList
  }

  private def readCompactBits(reader: TastyReader, bitsExpected: Int): List[Boolean] = {
    val bytesExpected = bitsExpected / 8 + (if (bitsExpected % 8 > 0) 1 else 0)
    val bytes = reader.readBytes(bytesExpected)
    bytes.toList.flatMap { bt =>
      List(
        (bt & 1) != 0,
        (bt & 2) != 0,
        (bt & 4) != 0,
        (bt & 8) != 0,
        (bt & 16) != 0,
        (bt & 32) != 0,
        (bt & 64) != 0,
        (bt & 128) != 0)
    }
  }

}
