package dotty.tools.backend.jvm

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.Trees.{PackageDef, ValDef}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Phases.Phase

import scala.collection.mutable
import scala.tools.asm.CustomAttr
import scala.tools.nsc.backend.jvm._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.interfaces
import java.util.Optional

import dotty.tools.dotc.core._
import dotty.tools.dotc.sbt.ExtractDependencies
import Contexts._
import Symbols._
import Decorators._

import java.io.DataOutputStream

import dotty.tools.io.Directory

import scala.tools.asm
import scala.tools.asm.tree._
import tpd._
import StdNames._
import dotty.tools.io._

class GenBCode extends Phase {
  def phaseName: String = GenBCode.name
  private val entryPoints = new mutable.HashSet[Symbol]()
  def registerEntryPoint(sym: Symbol) = entryPoints += sym

  private val superCallsMap = newMutableSymbolMap[Set[ClassSymbol]]
  def registerSuperCall(sym: Symbol, calls: ClassSymbol) = {
    val old = superCallsMap.getOrElse(sym, Set.empty)
    superCallsMap.update(sym, old + calls)
  }

  private[this] var myOutput: AbstractFile = _

  private def outputDir(implicit ctx: Context): AbstractFile = {
    if (myOutput eq null)
      myOutput = ctx.settings.outputDir.value
    myOutput
  }

  def run(implicit ctx: Context): Unit = {
    new GenBCodePipeline(entryPoints.toList,
        new DottyBackendInterface(outputDir, superCallsMap.toMap)(ctx))(ctx).run(ctx.compilationUnit.tpdTree)
    entryPoints.clear()
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context) = {
    try super.runOn(units)
    finally myOutput match {
      case jar: JarArchive =>
        jar.close()
      case _ =>
    }
  }
}

object GenBCode {
  val name: String = "genBCode"
}

class GenBCodePipeline(val entryPoints: List[Symbol], val int: DottyBackendInterface)(implicit val ctx: Context) extends BCodeSyncAndTry {

  var tree: Tree = _

  val sourceFile = ctx.compilationUnit.source

  /** Convert a `dotty.tools.io.AbstractFile` into a
   *  `dotty.tools.dotc.interfaces.AbstractFile`.
   */
  private[this] def convertAbstractFile(absfile: dotty.tools.io.AbstractFile): interfaces.AbstractFile =
    new interfaces.AbstractFile {
      override def name = absfile.name
      override def path = absfile.path
      override def jfile = Optional.ofNullable(absfile.file)
    }

  final class PlainClassBuilder(cunit: CompilationUnit) extends SyncAndTryBuilder(cunit)


//  class BCodePhase() {

    private[this] var bytecodeWriter  : BytecodeWriter   = null
    private[this] var mirrorCodeGen   : JMirrorBuilder   = null

    /* ---------------- q1 ---------------- */

    case class Item1(arrivalPos: Int, cd: TypeDef, cunit: CompilationUnit) {
      def isPoison = { arrivalPos == Int.MaxValue }
    }
    private val poison1 = Item1(Int.MaxValue, null, ctx.compilationUnit)
    private val q1 = new java.util.LinkedList[Item1]

    /* ---------------- q2 ---------------- */

    case class SubItem2(classNode: asm.tree.ClassNode,
                        file:      scala.tools.nsc.io.AbstractFile)

    case class Item2(arrivalPos: Int,
                     mirror:     SubItem2,
                     plain:      SubItem2) {
      def isPoison = { arrivalPos == Int.MaxValue }
    }

    private val poison2 = Item2(Int.MaxValue, null, null)
    private val q2 = new _root_.java.util.LinkedList[Item2]

    /* ---------------- q3 ---------------- */

    /*
     *  An item of queue-3 (the last queue before serializing to disk) contains three of these
     *  (one for each of mirror and plain classes).
     *
     *  @param jclassName  internal name of the class
     *  @param jclassBytes bytecode emitted for the class SubItem3 represents
     */
    case class SubItem3(
                         jclassName:  String,
                         jclassBytes: Array[Byte],
                         jclassFile:  scala.tools.nsc.io.AbstractFile
                         )

    case class Item3(arrivalPos: Int,
                     mirror:     SubItem3,
                     plain:      SubItem3) {

      def isPoison  = { arrivalPos == Int.MaxValue }
    }
    private val i3comparator = new java.util.Comparator[Item3] {
      override def compare(a: Item3, b: Item3) = {
        if (a.arrivalPos < b.arrivalPos) -1
        else if (a.arrivalPos == b.arrivalPos) 0
        else 1
      }
    }
    private val poison3 = Item3(Int.MaxValue, null, null)
    private val q3 = new java.util.PriorityQueue[Item3](1000, i3comparator)

    /*
     *  Pipeline that takes ClassDefs from queue-1, lowers them into an intermediate form, placing them on queue-2
     */
    class Worker1(needsOutFolder: Boolean) {

      private val lowerCaseNames = mutable.HashMap.empty[String, Symbol]
      private def checkForCaseConflict(javaClassName: String, classSymbol: Symbol) = {
        val lowerCaseName = javaClassName.toLowerCase
        lowerCaseNames.get(lowerCaseName) match {
          case None =>
            lowerCaseNames.put(lowerCaseName, classSymbol)
          case Some(dupClassSym) =>
            // Order is not deterministic so we enforce lexicographic order between the duplicates for error-reporting
            val (cl1, cl2) =
              if (classSymbol.effectiveName.toString < dupClassSym.effectiveName.toString) (classSymbol, dupClassSym)
              else (dupClassSym, classSymbol)
            ctx.atPhase(ctx.typerPhase) { implicit ctx =>
              ctx.warning(s"${cl1.show} differs only in case from ${cl2.showLocated}. " +
                "Such classes will overwrite one another on case-insensitive filesystems.", cl1.pos)
            }
        }
      }

      def run(): Unit = {
        while (true) {
          val item = q1.poll
          if (item.isPoison) {
            q2 add poison2
            return
          }
          else {
            try   { /*withCurrentUnit(item.cunit)*/(visit(item)) }
            catch {
              case ex: Throwable =>
                ex.printStackTrace()
                ctx.error(s"Error while emitting ${int.sourceFileFor(item.cunit)}\n${ex.getMessage}")
            }
          }
        }
      }

      /*
       *  Checks for duplicate internal names case-insensitively,
       *  builds ASM ClassNodes for mirror and plain classes;
       *  enqueues them in queue-2.
       *
       */
      def visit(item: Item1) = {
        val Item1(arrivalPos, cd, cunit) = item
        val claszSymbol = cd.symbol

        // -------------- mirror class, if needed --------------
        val mirrorC =
          if (int.symHelper(claszSymbol).isTopLevelModuleClass) {
            if (claszSymbol.companionClass == NoSymbol) {
              mirrorCodeGen.genMirrorClass(claszSymbol, cunit)
            } else {
              ctx.log(s"No mirror class for module with linked class: ${claszSymbol.fullName}")
              null
            }
          } else null

        // -------------- "plain" class --------------
        val pcb = new PlainClassBuilder(cunit)
        pcb.genPlainClass(cd)
        val outF = if (needsOutFolder) getOutFolder(claszSymbol, pcb.thisName) else null;
        val plainC = pcb.cnode

        if (claszSymbol.isClass) // @DarkDimius is this test needed here?
          for (binary <- ctx.compilationUnit.pickled.get(claszSymbol.asClass)) {
            val store = if (mirrorC ne null) mirrorC else plainC
            val tasty =
              if (!ctx.settings.YemitTastyInClass.value) {
                val outTastyFile = getFileForClassfile(outF, store.name, ".tasty")
                val outstream = new DataOutputStream(outTastyFile.bufferedOutput)
                try outstream.write(binary)
                finally outstream.close()
                // TASTY attribute is created but 0 bytes are stored in it.
                // A TASTY attribute has length 0 if and only if the .tasty file exists.
                Array.empty[Byte]
              } else {
                // Create an empty file to signal that a tasty section exist in the corresponding .class
                // This is much cheaper and simpler to check than doing classfile parsing
                getFileForClassfile(outF, store.name, ".hasTasty")
                binary
              }
            val dataAttr = new CustomAttr(nme.TASTYATTR.mangledString, tasty)
            store.visitAttribute(dataAttr)
          }


        // ----------- create files

        val classNodes = List(mirrorC, plainC)
        val classFiles = classNodes.map(cls =>
          if (outF != null && cls != null) {
            try {
              checkForCaseConflict(cls.name, claszSymbol)
              getFileForClassfile(outF, cls.name, ".class")
            } catch {
              case e: FileConflictException =>
                ctx.error(s"error writing ${cls.name}: ${e.getMessage}")
                null
            }
          } else null
        )

        // ----------- compiler and sbt's callbacks

        val (fullClassName, isLocal) = ctx.atPhase(ctx.sbtExtractDependenciesPhase) { implicit ctx =>
          (ExtractDependencies.classNameAsString(claszSymbol), claszSymbol.isLocal)
        }

        for ((cls, clsFile) <- classNodes.zip(classFiles)) {
          if (cls != null) {
            val className = cls.name.replace('/', '.')
            if (ctx.compilerCallback != null)
              ctx.compilerCallback.onClassGenerated(sourceFile, convertAbstractFile(clsFile), className)
            if (ctx.sbtCallback != null) {
              if (isLocal)
                ctx.sbtCallback.generatedLocalClass(sourceFile.jfile.orElse(null), clsFile.file)
              else {
                ctx.sbtCallback.generatedNonLocalClass(sourceFile.jfile.orElse(null), clsFile.file,
                  className, fullClassName)
              }
            }
          }
        }

        // ----------- hand over to pipeline-2

        val item2 =
          Item2(arrivalPos,
            SubItem2(mirrorC, classFiles(0)),
            SubItem2(plainC, classFiles(1)))

        q2 add item2 // at the very end of this method so that no Worker2 thread starts mutating before we're done.

      } // end of method visit(Item1)

    } // end of class BCodePhase.Worker1

    /*
     *  Pipeline that takes ClassNodes from queue-2. The unit of work depends on the optimization level:
     *
     *    (a) no optimization involves:
     *          - converting the plain ClassNode to byte array and placing it on queue-3
     */
    class Worker2 {
      // lazy val localOpt = new LocalOpt(new Settings())

      def localOptimizations(classNode: ClassNode): Unit = {
        // BackendStats.timed(BackendStats.methodOptTimer)(localOpt.methodOptimizations(classNode))
      }

      def run(): Unit = {
        while (true) {
          val item = q2.poll
          if (item.isPoison) {
            q3 add poison3
            return
          }
          else {
            try {
              localOptimizations(item.plain.classNode)
              addToQ3(item)
            } catch {
              case ex: Throwable =>
                ex.printStackTrace()
                ctx.error(s"Error while emitting ${item.plain.classNode.name}\n${ex.getMessage}")
            }
          }
        }
      }

      private def addToQ3(item: Item2) = {

        def getByteArray(cn: asm.tree.ClassNode): Array[Byte] = {
          val cw = new CClassWriter(extraProc)
          cn.accept(cw)
          cw.toByteArray
        }

        val Item2(arrivalPos, SubItem2(mirror, mirrorFile), SubItem2(plain, plainFile)) = item

        val mirrorC = if (mirror == null) null else SubItem3(mirror.name, getByteArray(mirror), mirrorFile)
        val plainC  = SubItem3(plain.name, getByteArray(plain), plainFile)

        if (AsmUtils.traceSerializedClassEnabled && plain.name.contains(AsmUtils.traceSerializedClassPattern)) {
          if (mirrorC != null) AsmUtils.traceClass(mirrorC.jclassBytes)
          AsmUtils.traceClass(plainC.jclassBytes)
        }

        q3 add Item3(arrivalPos, mirrorC, plainC)
      }

    } // end of class BCodePhase.Worker2

    var arrivalPos = 0

    /*
     *  A run of the BCodePhase phase comprises:
     *
     *    (a) set-up steps (most notably supporting maps in `BCodeTypes`,
     *        but also "the" writer where class files in byte-array form go)
     *
     *    (b) building of ASM ClassNodes, their optimization and serialization.
     *
     *    (c) tear down (closing the classfile-writer and clearing maps)
     *
     */
    def run(t: Tree) = {
      this.tree = t

      // val bcodeStart = Statistics.startTimer(BackendStats.bcodeTimer)

      // val initStart = Statistics.startTimer(BackendStats.bcodeInitTimer)
      arrivalPos = 0 // just in case
      // scalaPrimitives.init()
      bTypes.intializeCoreBTypes()
      // Statistics.stopTimer(BackendStats.bcodeInitTimer, initStart)

      // initBytecodeWriter invokes fullName, thus we have to run it before the typer-dependent thread is activated.
      bytecodeWriter  = initBytecodeWriter(entryPoints)
      mirrorCodeGen   = new JMirrorBuilder

      val needsOutfileForSymbol = bytecodeWriter.isInstanceOf[ClassBytecodeWriter]
      buildAndSendToDisk(needsOutfileForSymbol)

      // closing output files.
      bytecodeWriter.close()
      // Statistics.stopTimer(BackendStats.bcodeTimer, bcodeStart)

      if (ctx.compilerCallback != null)
        ctx.compilerCallback.onSourceCompiled(sourceFile)

      /* TODO Bytecode can be verified (now that all classfiles have been written to disk)
       *
       * (1) asm.util.CheckAdapter.verify()
       *       public static void verify(ClassReader cr, ClassLoader loader, boolean dump, PrintWriter pw)
       *     passing a custom ClassLoader to verify inter-dependent classes.
       *     Alternatively,
       *       - an offline-bytecode verifier could be used (e.g. Maxine brings one as separate tool).
       *       - -Xverify:all
       *
       * (2) if requested, check-java-signatures, over and beyond the syntactic checks in `getGenericSignature()`
       *
       */
    }

    /*
     *  Sequentially:
     *    (a) place all ClassDefs in queue-1
     *    (b) dequeue one at a time from queue-1, convert it to ASM ClassNode, place in queue-2
     *    (c) dequeue one at a time from queue-2, convert it to byte-array,    place in queue-3
     *    (d) serialize to disk by draining queue-3.
     */
    private def buildAndSendToDisk(needsOutFolder: Boolean) = {

      feedPipeline1()
      // val genStart = Statistics.startTimer(BackendStats.bcodeGenStat)
      (new Worker1(needsOutFolder)).run()
      // Statistics.stopTimer(BackendStats.bcodeGenStat, genStart)

      (new Worker2).run()

      // val writeStart = Statistics.startTimer(BackendStats.bcodeWriteTimer)
      drainQ3()
      // Statistics.stopTimer(BackendStats.bcodeWriteTimer, writeStart)

    }

    /* Feed pipeline-1: place all ClassDefs on q1, recording their arrival position. */
    private def feedPipeline1() = {
      def gen(tree: Tree): Unit = {
        tree match {
          case EmptyTree            => ()
          case PackageDef(_, stats) => stats foreach gen
          case ValDef(name, tpt, rhs) => () // module val not emitted
          case cd: TypeDef         =>
            q1 add Item1(arrivalPos, cd, int.currentUnit)
            arrivalPos += 1
        }
      }
      gen(tree)
      q1 add poison1
    }

    /* Pipeline that writes classfile representations to disk. */
    private def drainQ3() = {

      def sendToDisk(cfr: SubItem3): Unit = {
        if (cfr != null){
          val SubItem3(jclassName, jclassBytes, jclassFile) = cfr
          bytecodeWriter.writeClass(jclassName, jclassName, jclassBytes, jclassFile)
        }
      }

      var moreComing = true
      // `expected` denotes the arrivalPos whose Item3 should be serialized next
      var expected = 0

      while (moreComing) {
        val incoming = q3.poll
        moreComing   = !incoming.isPoison
        if (moreComing) {
          val item = incoming
          sendToDisk(item.mirror)
          sendToDisk(item.plain)
          expected += 1
        }
      }

      // we're done
      assert(q1.isEmpty, s"Some ClassDefs remained in the first queue: $q1")
      assert(q2.isEmpty, s"Some classfiles remained in the second queue: $q2")
      assert(q3.isEmpty, s"Some classfiles weren't written to disk: $q3")

    }
  //} // end of class BCodePhase
}
