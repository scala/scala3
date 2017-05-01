package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.{Literal, ref}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.{Symbol, defn}
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.core.tasty.DottyUnpickler.SummariesTreeSectionUnpickler
import dotty.tools.dotc.core.tasty.TastyUnpickler.NameTable
import dotty.tools.dotc.core.tasty._
import dotty.tools.dotc.transform.linker.types.{ClosureType, PreciseType}

import scala.collection.mutable


class TastySummaries {

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
    val topDenot = sym.topLevelClass.denot.asClass
    topDenot.dottyUnpickler.fold[List[MethodSummary]](Nil)(_.summaries)
  }

}


object TastySummaries {

  val version = 1

  val sectionName = "MethodSummaries"

  private val argsFlagsSize = 2

  def saveInTasty(methodSummaries: List[MethodSummary])(implicit ctx: Context): Unit = {
    for (cls <- ctx.compilationUnit.picklers.keySet) {
      val pickler = ctx.compilationUnit.picklers(cls)

      if (!ctx.scala2Mode) {
        val buf = new TastyBuffer(5000)
        val treePickl = pickler.treePkl
        val anchorTree = tpd.SyntheticValDef(sectionName.toTermName, Literal(Constant(sectionName)))

        treePickl.preRegister(anchorTree)
        treePickl.pickle(anchorTree :: Nil)

        pickler.newSection(sectionName, buf)
        val start = treePickl.buf.currentAddr

        val methods = methodSummaries.filter(_.methodDef.topLevelClass == cls)

        new SummaryWriter(treePickl, buf).write(methods)

        val sz = treePickl.buf.currentAddr.index - start.index

        ctx.debuglog("new section for " + cls + " size:"
          + sz + "/" + buf.currentAddr.index + " increased by " + (sz + buf.length) * 1.0 / start.index)
        // note: this is huge overestimate. This section contains a lot of refferences to already existing symbols and types
        // and will be compressed during bytecode generation by TreePickler.compactify

      }

      pickler.treePkl.compactify()
    }
  }

  class SummaryReader(tReader: SummariesTreeUnpickler#TreeReader, reader: TastyReader)(implicit ctx: Context) {

    def read(): List[MethodSummary] = {
      val version = reader.readInt()
      val methodsSz = reader.readInt()
      List.fill(methodsSz)(readMethodSummary(tReader, reader))
    }

    private def readSymbolRef = {
      val sym = tReader.readType()
      sym.termSymbol.orElse(sym.typeSymbol).orElse(sym.classSymbol)
    }

    private def readMethodSummary(tReader: SummariesTreeUnpickler#TreeReader, reader: TastyReader): MethodSummary = {
      val sym = readSymbolRef
      val methodsSz = reader.readInt()

      val methodsCalled = new mutable.HashMap[Type, List[CallInfo]]()

      for (_ <- 0 until methodsSz) {
        val preciseReceiver = reader.readByte() == 1 // TODO use a single compact bits section for all receivers before the loop
        val receiver = if (preciseReceiver) new PreciseType(tReader.readType()) else tReader.readType()

        val listSz = reader.readInt()
        methodsCalled(receiver) = List.fill(listSz)(readCallInfo)
      }

      val accessedModulesSz = reader.readInt()
      val accessedModules = List.fill(accessedModulesSz)(readSymbolRef)

      val argumentReturned = reader.readByte()

      val bitsExpected = reader.readByte()
      val (thisAccessed :: argumentStoredToHeap) = readCompactBits(reader, bitsExpected)

      val definedClosures = Nil // TODO

      new MethodSummary(sym, thisAccessed, methodsCalled.toMap, definedClosures, accessedModules, argumentReturned.toByte, argumentStoredToHeap.take(bitsExpected - 1))
    }

    private def readCallInfo: CallInfo = {
      val call = tReader.readType()

      val targsSz = reader.readByte()
      val targs = List.fill(targsSz)(tReader.readType())

      val argsSz = reader.readByte()
      val argsFlags = readCompactBits(reader, argsFlagsSize * argsSz)
      val argsPassed = argsFlags.sliding(argsFlagsSize, argsFlagsSize).map(readArg).toList

      val source = None // TODO

      CallInfo(call.asInstanceOf[TermRef], targs, argsPassed, source) // TODO no need to normalize the types
    }

    private def readArg(argFlags: List[Boolean]): Type = {
      assert(argFlags.length == argsFlagsSize)
      val precise :: closure :: _ = argFlags
      if (precise) new PreciseType(tReader.readType())
      else if (closure) new ClosureType(tReader.readTpt().asInstanceOf[tpd.Closure], tReader.readType(), readSymbolRef)
      else tReader.readType()
    }

    private def readCompactBits(reader: TastyReader, bitsExpected: Int): List[Boolean] = {
      val bytesExpected = bitsExpected / 8 + (if (bitsExpected % 8 > 0) 1 else 0)
      val bytes = reader.readBytes(bytesExpected)
      bytes.toIterator.flatMap { bt =>
        List((bt & 1) != 0, (bt & 2) != 0, (bt & 4) != 0, (bt & 8) != 0,
            (bt & 16) != 0, (bt & 32) != 0, (bt & 64) != 0, (bt & 128) != 0)
      }.take(bitsExpected).toList
    }
  }

  private class SummaryWriter(treePickl: TreePickler, buf: TastyBuffer)(implicit ctx: Context) {
    def write(methods: List[MethodSummary]) = {
      buf.writeInt(version) //1
      buf.writeInt(methods.length) // 19
      methods.foreach(serializeMethodSummary)
    }

    private def writeSymbolRef(sym: Symbol) =
      treePickl.pickleType(ref(sym).tpe)

    private def serializeMethodSummary(ms: MethodSummary) = {
      writeSymbolRef(ms.methodDef) //26

      buf.writeInt(ms.methodsCalled.size) //29
      for ((receiver, methods) <- ms.methodsCalled) {
        // TODO use a single compact bits section for all receivers before the loop
        buf.writeByte(if (receiver.isInstanceOf[PreciseType]) 1 else 0)
        val rec = receiver match {
          case receiver: PreciseType => receiver.underlying
          case _ => receiver
        }
        treePickl.pickleType(rec)
        buf.writeInt(methods.size)

        methods.foreach(writeCallInfo)
      }

      buf.writeInt(ms.accessedModules.length)
      ms.accessedModules foreach writeSymbolRef

      buf.writeByte(ms.argumentReturned)
      val flags = ms.thisAccessed :: ms.argumentStoredToHeap
      buf.writeByte(flags.length)
      writeCompactBits(flags)
    }

    private def writeCallInfo(c: CallInfo): Unit = {
      treePickl.pickleType(c.call)

      buf.writeByte(c.targs.size)
      c.targs.foreach(targ => treePickl.pickleType(targ))

      buf.writeByte(c.argumentsPassed.size)
      val argFlags =
        c.argumentsPassed.flatMap(arg => List(arg.isInstanceOf[PreciseType], arg.isInstanceOf[ClosureType]))
      assert(argFlags.size == argsFlagsSize * c.argumentsPassed.size)
      writeCompactBits(argFlags)
      c.argumentsPassed.foreach(writeArg)
    }

    private def writeArg(arg: Type): Unit = arg match {
      case arg: PreciseType =>
        treePickl.pickleType(arg.underlying)
      case arg: ClosureType =>
        treePickl.pickleTree(arg.meth)
        treePickl.pickleType(arg.u)
        writeSymbolRef(arg.implementedMethod)
      case arg =>
        treePickl.pickleType(arg)
    }

    private def writeCompactBits(ls: List[Boolean]): Unit = {
      def foldByte(bt: List[Boolean]) = bt.foldRight(0)((bl: Boolean, acc: Int) => (if (bl) 1 else 0) + (acc << 1))
      ls.grouped(8).map(foldByte).foreach(buf.writeByte)
    }
  }

}
