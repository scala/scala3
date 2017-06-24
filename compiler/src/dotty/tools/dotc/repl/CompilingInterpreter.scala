package dotty.tools
package dotc
package repl

import java.io.{
  File, PrintWriter, PrintStream, StringWriter, Writer, OutputStream,
  ByteArrayOutputStream => ByteOutputStream
}
import java.lang.{Class, ClassLoader, Thread, System, StringBuffer}
import java.net.{URL, URLClassLoader}

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, HashSet, ArrayBuffer}

//import ast.parser.SyntaxAnalyzer
import io.{PlainFile, VirtualDirectory}
import dotty.tools.io.{PlainDirectory, Directory}
import reporting.{ConsoleReporter, Reporter}
import core.Flags
import util.{SourceFile, NameTransformer}
import io.ClassPath
import ast.Trees._
import parsing.Parsers._
import core._
import dotty.tools.backend.jvm.GenBCode
import Symbols._, Types._, Contexts._, StdNames._, Names._, NameOps._
import Decorators._
import scala.util.control.NonFatal
import printing.SyntaxHighlighting

/** An interpreter for Scala code which is based on the `dotc` compiler.
 *
 *  The overall approach is based on compiling the requested code and then
 *  using a Java classloader and Java reflection to run the code
 *  and access its results.
 *
 *  In more detail, a single compiler instance is used
 *  to accumulate all successfully compiled or interpreted Scala code.  To
 *  "interpret" a line of code, the compiler generates a fresh object that
 *  includes the line of code and which has public definition(s) to export
 *  all variables defined by that code.  To extract the result of an
 *  interpreted line to show the user, a second "result object" is created
 *  which imports the variables exported by the above object and then
 *  exports a single definition named "result".  To accommodate user expressions
 *  that read from variables or methods defined in previous statements, "import"
 *  statements are used.
 *
 * This interpreter shares the strengths and weaknesses of using the
 *  full compiler-to-Java.  The main strength is that interpreted code
 *  behaves exactly as does compiled code, including running at full speed.
 *  The main weakness is that redefining classes and methods is not handled
 *  properly, because rebinding at the Java level is technically difficult.
 *
 * @author Moez A. Abdel-Gawad
 * @author Lex Spoon
 * @author Martin Odersky
 *
 * @param out   The output to use for diagnostics
 * @param ictx  The context to use for initialization of the interpreter,
 *              needed to access the current classpath.
 */
class CompilingInterpreter(
  out: PrintWriter,
  ictx: Context,
  parentClassLoader: Option[ClassLoader]
) extends Compiler with Interpreter {
  import ast.untpd._
  import CompilingInterpreter._

  ictx.base.initialize()(ictx)

  /** directory to save .class files to */
  val virtualDirectory =
    if (ictx.settings.d.isDefault(ictx)) new VirtualDirectory("(memory)", None)
    else new PlainDirectory(new Directory(new java.io.File(ictx.settings.d.value(ictx)))) // for now, to help debugging

  /** A GenBCode phase that uses `virtualDirectory` for its output */
  private class REPLGenBCode extends GenBCode {
    override def outputDir(implicit ctx: Context) = virtualDirectory
  }

  /** Phases of this compiler use `REPLGenBCode` instead of `GenBCode`. */
  override def phases = Phases.replace(
    classOf[GenBCode], _ => new REPLGenBCode :: Nil, super.phases)

  /** whether to print out result lines */
  private var printResults: Boolean = true
  private var delayOutput: Boolean = false

  val previousOutput = ListBuffer.empty[String]

  override def lastOutput() = {
    val prev = previousOutput.toList
    previousOutput.clear()
    prev
  }

  override def delayOutputDuring[T](operation: => T): T = {
    val old = delayOutput
    try {
      delayOutput = true
      operation
    } finally {
      delayOutput = old
    }
  }

  /** Temporarily be quiet */
  override def beQuietDuring[T](operation: => T): T = {
    val wasPrinting = printResults
    try {
      printResults = false
      operation
    } finally {
      printResults = wasPrinting
    }
  }

  private def newReporter =
    new ConsoleReporter(Console.in, out) {
      override def printMessage(msg: String) =
        if (!delayOutput) {
          out.print(/*clean*/(msg) + "\n")
          // Suppress clean for now for compiler messages
          // Otherwise we will completely delete all references to
          // line$object$ module classes. The previous interpreter did not
          // have the project because the module class was written without the final `$'
          // and therefore escaped the purge. We can turn this back on once
          // we drop the final `$' from module classes.
          out.flush()
        } else {
          previousOutput += (/*clean*/(msg) + "\n")
        }
    }

  /** the previous requests this interpreter has processed */
  private val prevRequests = new ArrayBuffer[Request]()

  /** the compiler's classpath, as URL's */
  val compilerClasspath: Seq[URL] = ictx.platform.classPath(ictx).asURLs

  /* A single class loader is used for all commands interpreted by this Interpreter.
     It would also be possible to create a new class loader for each command
     to interpret.  The advantages of the current approach are:

       - Expressions are only evaluated one time.  This is especially
         significant for I/O, e.g. "val x = Console.readLine"

     The main disadvantage is:

       - Objects, classes, and methods cannot be rebound.  Instead, definitions
         shadow the old ones, and old code objects refer to the old
         definitions.
  */
  /** class loader used to load compiled code */
  val classLoader: ClassLoader = {
    lazy val parent = new URLClassLoader(compilerClasspath.toArray,
                                         classOf[Interpreter].getClassLoader)

    new AbstractFileClassLoader(virtualDirectory, parentClassLoader.getOrElse(parent))
  }

  // Set the current Java "context" class loader to this interpreter's class loader
  Thread.currentThread.setContextClassLoader(classLoader)

  /** Parse a line into a sequence of trees. Returns None if the input is incomplete. */
  private def parse(line: String)(implicit ctx: Context): Option[List[Tree]] = {
    var justNeedsMore = false
    val reporter = newReporter
    reporter.withIncompleteHandler { _ => _ => justNeedsMore = true } {
      // simple parse: just parse it, nothing else
      def simpleParse(code: String)(implicit ctx: Context): List[Tree] = {
        val source = new SourceFile("<console>", code.toCharArray())
        val parser = new Parser(source)
        val (selfDef, stats) = parser.templateStatSeq()
        stats
      }
      val trees = simpleParse(line)(ctx.fresh.setReporter(reporter))
      if (reporter.hasErrors) {
        Some(Nil) // the result did not parse, so stop
      } else if (justNeedsMore) {
        None
      } else {
        Some(trees)
      }
    }
  }

  /** Compile a SourceFile.  Returns the root context of the run that compiled the file.
   */
  def compileSources(sources: List[SourceFile])(implicit ctx: Context): Context = {
    val reporter = newReporter
    val run = newRun(ctx.fresh.setReporter(reporter))
    run.compileSources(sources)
    run.runContext
  }

  /** Compile a string.  Returns true if there are no
   *  compilation errors, or false otherwise.
   */
  def compileString(code: String)(implicit ctx: Context): Boolean = {
    val runCtx = compileSources(List(new SourceFile("<script>", code.toCharArray)))
    !runCtx.reporter.hasErrors
  }

  override def interpret(line: String)(implicit ctx: Context): Interpreter.Result = {
    // if (prevRequests.isEmpty)
    //  new Run(this) // initialize the compiler // (not sure this is needed)
    // parse
    parse(line) match {
      case None => Interpreter.Incomplete
      case Some(Nil) => Interpreter.Error // parse error or empty input
      case Some(tree :: Nil) if tree.isTerm && !tree.isInstanceOf[Assign] =>
        previousOutput.clear() // clear previous error reporting
        interpret(s"val $newVarName =\n$line")
      case Some(trees) =>
        previousOutput.clear() // clear previous error reporting
        val req = new Request(line, newLineName)
        if (!req.compile())
          Interpreter.Error // an error happened during compilation, e.g. a type error
        else {
          val (resultStrings, succeeded) = req.loadAndRun()
          if (delayOutput)
            previousOutput ++= resultStrings.map(clean)
          else if (printResults || !succeeded)
            resultStrings.foreach(x => out.print(clean(x)))
          if (succeeded) {
            prevRequests += req
            Interpreter.Success
          }
          else Interpreter.Error
        }
    }
  }

  private def loadAndSetValue(objectName: String, value: AnyRef) = {
    /** This terrible string is the wrapped class's full name inside the
     *  classloader:
     *  lineX$object$$iw$$iw$list$object
     */
    val objName: String = List(
      currentLineName + INTERPRETER_WRAPPER_SUFFIX,
      INTERPRETER_IMPORT_WRAPPER,
      INTERPRETER_IMPORT_WRAPPER,
      objectName
    ).mkString("$")

    try {
      val resObj: Class[_] = Class.forName(objName, true, classLoader)
      val setMethod = resObj.getDeclaredMethods.find(_.getName == "set")

      setMethod.fold(false) { method =>
        method.invoke(resObj, value) == null
      }
    } catch {
      case NonFatal(_) =>
        // Unable to set value on object due to exception during reflection
        false
    }
  }

  /** This bind is implemented by creating an object with a set method and a
   *  field `value`. The value is then set via Java reflection.
   *
   *  Example: We want to bind a value `List(1,2,3)` to identifier `list` from
   *  sbt. The bind method accomplishes this by creating the following:
   *  {{{
   *    object ContainerObjectWithUniqueID {
   *      var value: List[Int] = _
   *      def set(x: Any) = value = x.asInstanceOf[List[Int]]
   *    }
   *    val list = ContainerObjectWithUniqueID.value
   *  }}}
   *
   *  Between the object being created and the value being assigned, the value
   *  inside the object is set via reflection.
   */
  override def bind(id: String, boundType: String, value: AnyRef)(implicit ctx: Context): Interpreter.Result =
    interpret(
      """
        |object %s {
        |  var value: %s = _
        |  def set(x: Any) = value = x.asInstanceOf[%s]
        |}
      """.stripMargin.format(id + INTERPRETER_WRAPPER_SUFFIX, boundType, boundType)
    ) match {
      case Interpreter.Success if loadAndSetValue(id + INTERPRETER_WRAPPER_SUFFIX, value) =>
        val line = "val %s = %s.value".format(id, id + INTERPRETER_WRAPPER_SUFFIX)
        interpret(line)
      case Interpreter.Error | Interpreter.Incomplete =>
        out.println("Set failed in bind(%s, %s, %s)".format(id, boundType, value))
        Interpreter.Error
    }

  /** Trait collecting info about one of the statements of an interpreter request */
  private trait StatementInfo {
    /** The statement */
    def statement: Tree

    /** The names defined previously and referred to in the statement */
    def usedNames: List[Name]

    /** The names defined in the statement */
    val boundNames: List[Name]

    /** Statement is an import that contains a wildcard */
    val importsWildcard: Boolean

    /** The names imported by the statement (if it is an import clause) */
    val importedNames: Seq[Name]

    /** Statement defines an implicit calue or method */
    val definesImplicit: Boolean
  }

  /** One line of code submitted by the user for interpretation */
  private class Request(val line: String, val lineName: String)(implicit ctx: Context) {
    private val trees = {
      val parsed = parse(line)
      previousOutput.clear() // clear previous error reporting
      parsed match {
        case Some(ts) => ts
        case None => Nil
      }
    }

    /** name to use for the object that will compute "line" */
    private def objectName = lineName + INTERPRETER_WRAPPER_SUFFIX

    /** name of the object that retrieves the result from the above object */
    private def resultObjectName = "RequestResult$" + objectName

    private def chooseHandler(stat: Tree): StatementHandler = stat match {
      case stat: DefDef => new DefHandler(stat)
      case stat: ValDef => new ValHandler(stat)
      case stat: PatDef => new PatHandler(stat)
      case stat @ Assign(Ident(_), _) => new AssignHandler(stat)
      case stat: ModuleDef => new ModuleHandler(stat)
      case stat: TypeDef if stat.isClassDef => new ClassHandler(stat)
      case stat: TypeDef => new TypeAliasHandler(stat)
      case stat: Import => new ImportHandler(stat)
//    case DocDef(_, documented) => chooseHandler(documented)
      case stat => new GenericHandler(stat)
    }

    private val handlers: List[StatementHandler] = trees.map(chooseHandler)

    /** all (public) names defined by these statements */
    private val boundNames = ListSet(handlers.flatMap(_.boundNames): _*).toList

    /** list of names used by this expression */
    private val usedNames: List[Name] = handlers.flatMap(_.usedNames)

    private val (importsPreamble, importsTrailer, accessPath) =
      importsCode(usedNames.toSet)

    /** Code to access a variable with the specified name */
    private def fullPath(vname: String): String = s"$objectName$accessPath.`$vname`"

    /** Code to access a variable with the specified name */
    private def fullPath(vname: Name): String = fullPath(vname.toString)

    /** the line of code to compute */
    private def toCompute = line

    /** generate the source code for the object that computes this request
     *  TODO Reformulate in a functional way
     */
    private def objectSourceCode: String =
      stringFrom { code =>
        // header for the wrapper object
        code.println(s"object $objectName{")
        code.print(importsPreamble)
        code.println(toCompute)
        handlers.foreach(_.extraCodeToEvaluate(this,code))
        code.println(importsTrailer)
        //end the wrapper object
        code.println(";}")
      }

    /** Types of variables defined by this request.  They are computed
        after compilation of the main object */
    private var typeOf: Map[Name, String] = _

    /** generate source code for the object that retrieves the result
        from objectSourceCode */
    private def resultObjectSourceCode: String =
      stringFrom(code => {
        code.println(s"object $resultObjectName")
        code.println("{ val result: String = {")
        code.println(s"$objectName$accessPath;")  // evaluate the object, to make sure its constructor is run
        code.print("(\"\"")  // print an initial empty string, so later code can
                            // uniformly be: + morestuff
        handlers.foreach(_.resultExtractionCode(this, code))
        code.println("\n)}")
        code.println(";}")
      })


    /** Compile the object file.  Returns whether the compilation succeeded.
     *  If all goes well, the "types" map is computed. */
    def compile(): Boolean = {
      val compileCtx = compileSources(
        List(new SourceFile("<console>", objectSourceCode.toCharArray)))
      !compileCtx.reporter.hasErrors && {
        this.typeOf = findTypes(compileCtx)
        val resultCtx = compileSources(
          List(new SourceFile("<console>", resultObjectSourceCode.toCharArray)))
        !resultCtx.reporter.hasErrors
      }
    }

    /** Dig the types of all bound variables out of the compiler run.
     *  TODO: Change the interface so that we typecheck, and then transform
     *  directly. Treating the compiler as less of a blackbox will require
     *  much less magic here.
     */
    private def findTypes(implicit ctx: Context): Map[Name, String] = {
      def valAndVarNames = handlers.flatMap(_.valAndVarNames)
      def defNames = handlers.flatMap(_.defNames)

      def getTypes(names: List[Name], nameMap: Name => Name): Map[Name, String] = {
        /** the outermost wrapper object */
        val outerResObjSym: Symbol =
          defn.EmptyPackageClass.info.decl(objectName.toTermName).symbol

        /** the innermost object inside the wrapper, found by
          * following accessPath into the outer one. */
        val resObjSym =
          (accessPath.split("\\.")).foldLeft(outerResObjSym) { (sym,str) =>
            if (str.isEmpty) sym
            else
              ctx.atPhase(ctx.typerPhase.next) { implicit ctx =>
                sym.info.member(str.toTermName).symbol
              }
          }

        names.foldLeft(Map.empty[Name,String]) { (map, name) =>
          val rawType =
            ctx.atPhase(ctx.typerPhase.next) { implicit ctx =>
              resObjSym.info.member(name).info
            }

          // the types are all =>T; remove the =>
          val cleanedType = rawType.widenExpr match {
            case tp: MethodType => tp.resultType
            case tp => tp
          }

          map + (name ->
            ctx.atPhase(ctx.typerPhase.next) { implicit ctx =>
              cleanedType.show
            })
        }
      }

      val names1 = getTypes(valAndVarNames, n => n.toTermName.fieldName)
      val names2 = getTypes(defNames, identity)
      names1 ++ names2
    }

    /** Sets both System.{out,err} and Console.{out,err} to supplied
     *  `os: OutputStream`
     */
    private def withOutput[T](os: ByteOutputStream)(op: ByteOutputStream => T) = {
      val ps     = new PrintStream(os)
      val oldOut = System.out
      val oldErr = System.err
      System.setOut(ps)
      System.setErr(ps)

      try {
        Console.withOut(os)(Console.withErr(os)(op(os)))
      } finally {
        System.setOut(oldOut)
        System.setErr(oldErr)
      }
    }

    /** load and run the code using reflection.
     *  @return  A pair consisting of the run's result as a `List[String]`, and
     *           a boolean indicating whether the run succeeded without throwing
     *           an exception.
     */
    def loadAndRun(): (List[String], Boolean) = {
      val interpreterResultObject: Class[_] =
        Class.forName(resultObjectName, true, classLoader)
      val valMethodRes: java.lang.reflect.Method =
        interpreterResultObject.getMethod("result")
      try {
        withOutput(new ByteOutputStream) { ps =>
          val rawRes = valMethodRes.invoke(interpreterResultObject).toString
          val res =
            if (ictx.useColors) new String(SyntaxHighlighting(rawRes).toArray)
            else rawRes
          val prints = ps.toString("utf-8")
          val printList = if (prints != "") prints :: Nil else Nil

          if (!delayOutput) out.print(prints)

          (printList :+ res, true)
        }
      } catch {
        case NonFatal(ex) =>
          def cause(ex: Throwable): Throwable =
            if (ex.getCause eq null) ex else cause(ex.getCause)
          val orig = cause(ex)
          (stringFrom(str => orig.printStackTrace(str)) :: Nil, false)
      }
    }

    /** Compute imports that allow definitions from previous
     *  requests to be visible in a new request.  Returns
     *  three pieces of related code as strings:
     *
     *  1. A _preamble_: An initial code fragment that should go before
     *  the code of the new request.
     *
     *  2. A _trailer_: A code fragment that should go after the code
     *  of the new request.
     *
     *  3. An _access path_ which can be traversed to access
     *  any bindings inside code wrapped by #1 and #2 .
     *
     *  The argument is a set of Names that need to be imported.
     *
     *  Limitations: This method is not as precise as it could be.
     *  (1) It does not process wildcard imports to see what exactly
     *  they import.
     *  (2) If it imports any names from a request, it imports all
     *  of them, which is not really necessary.
     *  (3) It imports multiple same-named implicits, but only the
     *  last one imported is actually usable.
     */
    private def importsCode(wanted: Set[Name]): (String, String, String) = {
      /** Narrow down the list of requests from which imports
       *  should be taken.  Removes requests which cannot contribute
       *  useful imports for the specified set of wanted names.
       */
      def reqsToUse: List[(Request, StatementInfo)] = {
        /** Loop through a list of StatementHandlers and select
         *  which ones to keep.  'wanted' is the set of
         *  names that need to be imported.
         */
        def select(reqs: List[(Request, StatementInfo)], wanted: Set[Name]): List[(Request, StatementInfo)] = {
          reqs match {
            case Nil => Nil

            case (req, handler) :: rest =>
              val keepit =
                (handler.definesImplicit ||
                  handler.importsWildcard ||
                  handler.importedNames.exists(wanted.contains(_)) ||
                  handler.boundNames.exists(wanted.contains(_)))

              val newWanted =
                if (keepit) {
                  (wanted
                    ++ handler.usedNames
                    -- handler.boundNames
                    -- handler.importedNames)
                } else {
                  wanted
                }

              val restToKeep = select(rest, newWanted)

              if (keepit)
                (req, handler) :: restToKeep
              else
                restToKeep
          }
        }

        val rhpairs = for {
          req <- prevRequests.toList.reverse
          handler <- req.handlers
        } yield (req, handler)

        select(rhpairs, wanted).reverse
      }

      val preamble = new StringBuffer
      val trailingBraces = new StringBuffer
      val accessPath = new StringBuffer
      val impname = INTERPRETER_IMPORT_WRAPPER
      val currentImps = mutable.Set[Name]()

      // add code for a new object to hold some imports
      def addWrapper(): Unit = {
        preamble.append("object " + impname + "{\n")
        trailingBraces.append("}\n")
        accessPath.append("." + impname)
        currentImps.clear()
      }

      addWrapper()

      // loop through previous requests, adding imports
      // for each one
      for ((req, handler) <- reqsToUse) {
        // If the user entered an import, then just use it

        // add an import wrapping level if the import might
        // conflict with some other import
        if (handler.importsWildcard ||
          currentImps.exists(handler.importedNames.contains))
          if (!currentImps.isEmpty)
            addWrapper()

        if (handler.statement.isInstanceOf[Import])
          preamble.append(handler.statement.show + ";\n")

        // give wildcard imports a import wrapper all to their own
        if (handler.importsWildcard)
          addWrapper()
        else
          currentImps ++= handler.importedNames

        // For other requests, import each bound variable.
        // import them explicitly instead of with _, so that
        // ambiguity errors will not be generated. Also, quote
        // the name of the variable, so that we don't need to
        // handle quoting keywords separately.
        for (imv <- handler.boundNames) {
          if (currentImps.contains(imv))
            addWrapper()
          preamble.append("import ")
          preamble.append(req.objectName + req.accessPath + ".`" + imv + "`;\n")
          currentImps += imv
        }
      }

      addWrapper() // Add one extra wrapper, to prevent warnings
      // in the frequent case of redefining
      // the value bound in the last interpreter
      // request.

      (preamble.toString, trailingBraces.toString, accessPath.toString)
    }

    // ------ Handlers ------------------------------------------

    /** Class to handle one statement among all the statements included
     *  in a single interpreter request.
     */
    private sealed abstract class StatementHandler(val statement: Tree) extends StatementInfo {
      val usedNames: List[Name] = {
        val ivt = new UntypedTreeAccumulator[mutable.Set[Name]] {
          override def apply(ns: mutable.Set[Name], tree: Tree)(implicit ctx: Context) =
            tree match {
              case Ident(name) => ns += name
              case _ => foldOver(ns, tree)
            }
        }
        ivt.foldOver(HashSet(), statement).toList
      }
      val boundNames: List[Name] = Nil
      def valAndVarNames: List[Name] = Nil
      def defNames: List[Name] = Nil
      val importsWildcard = false
      val importedNames: Seq[Name] = Nil
      val definesImplicit = statement match {
        case tree: MemberDef => tree.mods.is(Flags.Implicit)
        case _ => false
      }

      def extraCodeToEvaluate(req: Request, code: PrintWriter) = {}
      def resultExtractionCode(req: Request, code: PrintWriter) = {}
    }

    private class GenericHandler(statement: Tree) extends StatementHandler(statement)

    private abstract class ValOrPatHandler(statement: Tree)
        extends StatementHandler(statement) {
      override val boundNames: List[Name] = _boundNames
      override def valAndVarNames = boundNames

      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        if (!shouldShowResult(req)) return
        val resultExtractors = boundNames.map(name => resultExtractor(req, name))
        code.print(resultExtractors.mkString(""))
      }

      private val ListReg = """^.*List\[(\w+)\]$""".r
      private val MapReg = """^.*Map\[(\w+),[ ]*(\w+)\]$""".r
      private val LitReg = """^.*\((.+)\)$""".r

      private def resultExtractor(req: Request, varName: Name): String = {
        val prettyName = varName.decode
        // FIXME: `varType` is prettified to abbreviate common types where
        // appropriate, and to also prettify literal types
        //
        // This should be rewritten to use the actual types once we have a
        // semantic representation available to the REPL
        val varType = string2code(req.typeOf(varName)) match {
          // Extract List's paremeter from full path
          case ListReg(param) => s"List[$param]"
          // Extract Map's paremeters from full path
          case MapReg(k, v) => s"Map[$k, $v]"
          // Extract literal type from literal type representation. Example:
          //
          // ```
          // scala> val x: 42 = 42
          // val x: Int(42) = 42
          // scala> val y: "hello" = "hello"
          // val y: String("hello") = "hello"
          // ```
          case LitReg(lit) => lit
          // When the type is a singleton value like None, don't show `None$`
          // instead show `None.type`.
          case x if x.lastOption == Some('$') => x.init + ".type"
          case x => x
        }
        val fullPath = req.fullPath(varName)

        val varOrVal = statement match {
          case v: ValDef if v.mods is Flags.Mutable => "var"
          case _ => "val"
        }

        s""" + "$varOrVal $prettyName: $varType = " + {
           |  if ($fullPath.asInstanceOf[AnyRef] != null) {
           |    (if ($fullPath.toString().contains('\\n')) "\\n" else "") + {
           |      import dotty.Show._
           |      $fullPath.show /*toString()*/ + "\\n"
           |    }
           |  } else {
           |    "null\\n"
           |  }
           |}""".stripMargin
      }

      protected def _boundNames: List[Name]
      protected def shouldShowResult(req: Request): Boolean
    }

    private class ValHandler(statement: ValDef) extends ValOrPatHandler(statement) {
      override def _boundNames = List(statement.name)

      override def shouldShowResult(req: Request): Boolean =
        !statement.mods.is(Flags.AccessFlags) &&
          !(isGeneratedVarName(statement.name.toString) &&
            req.typeOf(statement.name) == "Unit")
    }


    private class PatHandler(statement: PatDef) extends ValOrPatHandler(statement) {
      override def _boundNames = statement.pats.flatMap(findVariableNames)

      override def shouldShowResult(req: Request): Boolean =
        !statement.mods.is(Flags.AccessFlags)

      private def findVariableNames(tree: Tree): List[Name] = tree match {
        case Ident(name) if name.toString != "_" => List(name)
        case _ => VariableNameFinder(Nil, tree).reverse
      }

      private object VariableNameFinder extends UntypedDeepFolder[List[Name]](
        (acc: List[Name], t: Tree) => t match {
          case _: BackquotedIdent => acc
          case Ident(name) if name.isVariableName && name.toString != "_" => name :: acc
          case Bind(name, _) if name.isVariableName => name :: acc
          case _ => acc
        }
      )
    }

    private class DefHandler(defDef: DefDef) extends StatementHandler(defDef) {
      override val boundNames = List(defDef.name)
      override def defNames = boundNames

      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        /** TODO: This is the result of the state of the REPL - this would be
          * entirely unnecessary with a better structure where we could just
          * use the type printer
          *
          * @see `def findTypes` for an explanation of what should be done
          */
        if (!defDef.mods.is(Flags.AccessFlags)) {
          // Take the DefDef and remove the `rhs` and ascribed type `tpt`
          val copy = ast.untpd.cpy.DefDef(defDef)(
            rhs = EmptyTree,
            tpt = TypeTree()
          )

          val tpt = defDef.tpt match {
            // ascribed TypeExpr e.g: `def foo: Int = 5`
            case Ident(tpt) if defDef.vparamss.isEmpty =>
              ": " + tpt.show
            case tpt =>
              ": " + req.typeOf(defDef.name)
          }
          code.print {
            "+\"" + string2code(copy.show) + tpt + "\\n\""
          }
        }
      }
    }

    private class AssignHandler(statement: Assign) extends StatementHandler(statement) {
      val lhs = statement.lhs.asInstanceOf[Ident] // an unfortunate limitation

      val helperName = newInternalVarName().toTermName
      override val valAndVarNames = List(helperName)

      override def extraCodeToEvaluate(req: Request, code: PrintWriter): Unit = {
        code.println(i"val $helperName = ${statement.lhs};")
      }

      /** Print out lhs instead of the generated varName */
      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        code.print(" + \"" + lhs.show + ": " +
          string2code(req.typeOf(helperName)) +
          " = \" + " +
          string2code(req.fullPath(helperName))
          + " + \"\\n\"")
      }
    }

    private class ModuleHandler(module: ModuleDef) extends StatementHandler(module) {
      override val boundNames = List(module.name)

      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        code.println(" + \"defined module " +
          string2code(module.name.toString)
          + "\\n\"")
      }
    }

    private class ClassHandler(classdef: TypeDef)
      extends StatementHandler(classdef) {
      override val boundNames =
        List(classdef.name) :::
          (if (classdef.mods.is(Flags.Case))
            List(classdef.name.toTermName)
          else
            Nil)

      // TODO: MemberDef.keyword does not include "trait";
      // otherwise it could be used here
      def keyword: String =
        if (classdef.mods.is(Flags.Trait)) "trait" else "class"

      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        code.print(
          " + \"defined " +
            keyword +
            " " +
            string2code(classdef.name.toString) +
            "\\n\"")
      }
    }

    private class TypeAliasHandler(typeDef: TypeDef)
      extends StatementHandler(typeDef) {
      override val boundNames =
        if (!typeDef.mods.is(Flags.AccessFlags) && !typeDef.rhs.isInstanceOf[TypeBoundsTree])
          List(typeDef.name)
        else
          Nil

      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        code.println(" + \"defined type alias " +
          string2code(typeDef.name.toString) + "\\n\"")
      }
    }

    private class ImportHandler(imp: Import) extends StatementHandler(imp) {
      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        code.println("+ \"" + imp.show + "\\n\"")
      }

      def isWildcardSelector(tree: Tree) = tree match {
        case Ident(nme.USCOREkw) => true
        case _ => false
      }

      /** Whether this import includes a wildcard import */
      override val importsWildcard = imp.selectors.exists(isWildcardSelector)

      /** The individual names imported by this statement */
      override val importedNames: Seq[Name] =
        imp.selectors.filterNot(isWildcardSelector).flatMap {
          case sel: RefTree => List(sel.name.toTypeName, sel.name.toTermName)
          case _ => Nil
        }
    }

  } // end Request

  // ------- String handling ----------------------------------

  /** next line number to use */
  private var nextLineNo = 0

  /** allocate a fresh line name */
  private def newLineName = {
    val num = nextLineNo
    nextLineNo += 1
    INTERPRETER_LINE_PREFIX + num
  }

  private def currentLineName =
    INTERPRETER_LINE_PREFIX + (nextLineNo - 1)

  /** next result variable number to use */
  private var nextVarNameNo = 0

  /** allocate a fresh variable name */
  private def newVarName = {
    val num = nextVarNameNo
    nextVarNameNo += 1
    INTERPRETER_VAR_PREFIX + num
  }

  /** next internal variable number to use */
  private var nextInternalVarNo = 0

  /** allocate a fresh internal variable name */
  private def newInternalVarName() = {
    val num = nextVarNameNo
    nextVarNameNo += 1
    INTERPRETER_SYNTHVAR_PREFIX + num
  }

  /** Check if a name looks like it was generated by newVarName */
  private def isGeneratedVarName(name: String): Boolean =
    name.startsWith(INTERPRETER_VAR_PREFIX) && {
      val suffix = name.drop(INTERPRETER_VAR_PREFIX.length)
      suffix.forall(_.isDigit)
    }

  /** generate a string using a routine that wants to write on a stream */
  private def stringFrom(writer: PrintWriter => Unit): String = {
    val stringWriter = new StringWriter()
    val stream = new NewLinePrintWriter(stringWriter)
    writer(stream)
    stream.close()
    stringWriter.toString
  }

  /** Truncate a string if it is longer than settings.maxPrintString */
  private def truncPrintString(str: String)(implicit ctx: Context): String = {
    val maxpr = ctx.settings.XreplLineWidth.value

    if (maxpr <= 0)
      return str

    if (str.length <= maxpr)
      return str

    val trailer = "..."
    if (maxpr >= trailer.length-1)
      str.substring(0, maxpr-3) + trailer + "\n"
    else
      str.substring(0, maxpr-1)
  }

  /** Clean up a string for output */
  private def clean(str: String)(implicit ctx: Context) =
    truncPrintString(stripWrapperGunk(str))
}

/** Utility methods for the Interpreter. */
object CompilingInterpreter {
  val INTERPRETER_WRAPPER_SUFFIX = "$object"
  val INTERPRETER_LINE_PREFIX = "line"
  val INTERPRETER_VAR_PREFIX = "res"
  val INTERPRETER_IMPORT_WRAPPER = "$iw"
  val INTERPRETER_SYNTHVAR_PREFIX = "synthvar$"

  /** Delete a directory tree recursively.  Use with care!
   */
  private[repl] def deleteRecursively(path: File): Unit = {
    path match  {
      case _ if !path.exists =>
        ()
      case _ if path.isDirectory =>
        for (p <- path.listFiles)
          deleteRecursively(p)
        path.delete
      case _ =>
        path.delete
    }
  }

  /** Heuristically strip interpreter wrapper prefixes
   *  from an interpreter output string.
   */
  def stripWrapperGunk(str: String): String = {
    val wrapregex = "(line[0-9]+\\$object[$.])?(\\$iw[$.])*"
    str.replaceAll(wrapregex, "")
  }

  /** Convert a string into code that can recreate the string.
   *  This requires replacing all special characters by escape
   *  codes. It does not add the surrounding " marks.  */
  def string2code(str: String): String = {
    /** Convert a character to a backslash-u escape */
    def char2uescape(c: Char): String = {
      var rest = c.toInt
      val buf = new StringBuilder
      for (i <- 1 to 4) {
	      buf ++= (rest % 16).toHexString
	      rest = rest / 16
      }
      "\\" + "u" + buf.toString.reverse
    }
    val res = new StringBuilder
    for (c <- str) {
      if ("'\"\\" contains c) {
	      res += '\\'
	      res += c
      } else if (!c.isControl) {
	      res += c
      } else {
	      res ++= char2uescape(c)
      }
    }
    res.toString
  }
}
