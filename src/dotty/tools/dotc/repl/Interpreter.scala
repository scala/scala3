package dotty.tools
package dotc
package repl

import java.io.{File, PrintWriter, StringWriter, Writer}
import java.lang.{Class, ClassLoader}
import java.net.{URL, URLClassLoader}

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, HashSet, ArrayBuffer}

//import ast.parser.SyntaxAnalyzer
import io.{PlainFile, VirtualDirectory}
import reporting.{ConsoleReporter, Reporter}
import core.Flags
import util.{SourceFile, NameTransformer}
import io.ClassPath
import repl.{InterpreterResults=>IR}
import ast.Trees._
import parsing.Parsers._
import core._
import dotty.tools.backend.jvm.GenBCode
import Symbols._, Types._, Contexts._, StdNames._, Names._, NameOps._
import Decorators._
import Interpreter._

/** <p>
 *    An interpreter for Scala code.
 *  </p>
 *  <p>
 *    The main public entry points are <code>compile()</code> and
 *    <code>interpret()</code>. The <code>compile()</code> method loads a
 *    complete Scala file.  The <code>interpret()</code> method executes one
 *    line of Scala code at the request of the user.
 *  </p>
 *  <p>
 *    The overall approach is based on compiling the requested code and then
 *    using a Java classloader and Java reflection to run the code
 *    and access its results.
 *  </p>
 *  <p>
 *    In more detail, a single compiler instance is used
 *    to accumulate all successfully compiled or interpreted Scala code.  To
 *    "interpret" a line of code, the compiler generates a fresh object that
 *    includes the line of code and which has public member(s) to export
 *    all variables defined by that code.  To extract the result of an
 *    interpreted line to show the user, a second "result object" is created
 *    which imports the variables exported by the above object and then
 *    exports a single member named "result".  To accomodate user expressions
 *    that read from variables or methods defined in previous statements, "import"
 *    statements are used.
 *  </p>
 *  <p>
 *    This interpreter shares the strengths and weaknesses of using the
 *    full compiler-to-Java.  The main strength is that interpreted code
 *    behaves exactly as does compiled code, including running at full speed.
 *    The main weakness is that redefining classes and methods is not handled
 *    properly, because rebinding at the Java level is technically difficult.
 *  </p>
 *
 * @author Moez A. Abdel-Gawad
 * @author Lex Spoon
 */
class Interpreter(out: PrintWriter, ictx: Context) extends Compiler {

  import ast.untpd._
  import Interpreter._

  /** directory to save .class files to */
  val virtualDirectory = new VirtualDirectory("(memory)", None)

  class REPLGenBCode extends GenBCode {
    override def outputDir(implicit ctx: Context) = virtualDirectory
  }

  override def phases = Phases.replace(
      classOf[GenBCode], _ => new REPLGenBCode :: Nil, super.phases)

  /** whether to print out result lines */
  private var printResults: Boolean = true

  /** Be quiet.  Do not print out the results of each
    * submitted command unless an exception is thrown.  */
  def beQuiet = { printResults = false }

  /** Temporarily be quiet */
  def beQuietDuring[T](operation: => T): T = {
    val wasPrinting = printResults
    try {
      printResults = false
      operation
    } finally {
      printResults = wasPrinting
    }
  }

  /** interpreter settings */
  val isettings = new InterpreterSettings

  def newReporter = new ConsoleReporter(Console.in, writer = out) {
    //override def printMessage(msg: String) { out.println(clean(msg)) }
    override def printMessage(msg: String) = {
      out.print(clean(msg) + "\n")
      out.flush()
    }
  }

  /** the previous requests this interpreter has processed */
  private val prevRequests = new ArrayBuffer[Request]()

  /** the compiler's classpath, as URL's */
  val compilerClasspath: List[URL] = ictx.platform.classPath(ictx).asURLs

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
    val parent = new URLClassLoader(compilerClasspath.toArray, parentClassLoader)
    new AbstractFileClassLoader(virtualDirectory, parent)
  }

  protected def parentClassLoader: ClassLoader = classOf[Interpreter].getClassLoader

  /** Set the current Java "context" class loader to this
   *  interpreter's class loader
   */
  def setContextClassLoader(): Unit = {
    Thread.currentThread.setContextClassLoader(classLoader)
  }

  /** Parse a line into a sequence of trees. Returns None if the input
   *  is incomplete.
   */
  private def parse(line: String)(implicit ctx: Context): Option[List[Tree]] = {
    var justNeedsMore = false
    val reporter = newReporter
    reporter.withIncompleteHandler { _ => _ => justNeedsMore = true } {
      // simple parse: just parse it, nothing else
      def simpleParse(code: String): List[Tree] = {
        val source = new SourceFile("<console>", code.toCharArray())
        val parser = new Parser(source)
        val (selfDef, stats) = parser.templateStatSeq
        stats
      }
      val trees = simpleParse(line)
      if (reporter.hasErrors) {
        Some(Nil) // the result did not parse, so stop
      } else if (justNeedsMore) {
        None
      } else {
        Some(trees)
      }
    }
  }

  /** Compile a SourceFile.  Returns true if there are
   *  no compilation errors, or false othrewise.
   */
  def compileSources(sources: List[SourceFile])(implicit ctx: Context): Boolean = {
    val reporter = newReporter
    val run = new Run(this)(ctx.fresh.setReporter(reporter))
    run.compileSources(sources)
    !reporter.hasErrors
  }

  /** Compile a string.  Returns true if there are no
   *  compilation errors, or false otherwise.
   */
  def compileString(code: String)(implicit ctx: Context): Boolean =
    compileSources(List(new SourceFile("<script>", code.toCharArray)))

  /** <p>
   *    Interpret one line of input.  All feedback, including parse errors
   *    and evaluation results, are printed via the supplied compiler's
   *    reporter.  Values defined are available for future interpreted
   *    strings.
   *  </p>
   *  <p>
   *    The return value is whether the line was interpreter successfully,
   *    e.g. that there were no parse errors.
   *  </p>
   *
   *  @param line ...
   *  @return     ...
   */
  def interpret(line: String)(implicit ctx: Context): IR.Result = {
    if (prevRequests.isEmpty)
      new Run(this) // initialize the compiler

    // parse
    val trees = parse(indentCode(line)) match {
      case None => return IR.Incomplete
      case Some(Nil) => return IR.Error // parse error or empty input
      case Some(trees) => trees
    }

    trees match {
      case List(_: Assign) => ()

      case List(_: TermTree) | List(_: Ident) | List(_: Select) =>
        // Treat a single expression specially.
        // This is necessary due to it being hard to modify
        // code at a textual level, and it being hard to
        // submit an AST to the compiler.
        return interpret("val " + newVarName() + " = \n" + line)

      case _ => ()
    }

    val lineName = newLineName

    // figure out what kind of request
    val req = buildRequest(trees, line, lineName)
    if (req eq null) return IR.Error // a disallowed statement type

    if (!req.compile)
      return IR.Error // an error happened during compilation, e.g. a type error

    val (interpreterResultString, succeeded) = req.loadAndRun

    if (printResults || !succeeded) {
      // print the result
      out.print(clean(interpreterResultString))
    }

    // book-keeping
    if (succeeded)
      prevRequests += req

    if (succeeded) IR.Success else IR.Error
  }

  /** A counter used for numbering objects created by <code>bind()</code>. */
  private var binderNum = 0

  /** Bind a specified name to a specified value.  The name may
   *  later be used by expressions passed to interpret.
   *
   *  @param name      the variable name to bind
   *  @param boundType the type of the variable, as a string
   *  @param value     the object value to bind to it
   *  @return          an indication of whether the binding succeeded
   */
  def bind(name: String, boundType: String, value: Any)(implicit ctx: Context): IR.Result = {
    val binderName = "binder" + binderNum
    binderNum += 1

    compileString(
      "object " + binderName +
        "{ var value: " + boundType + " = _; " +
        " def set(x: Any) = value=x.asInstanceOf[" + boundType + "]; }")

    val binderObject =
      Class.forName(binderName, true, classLoader)
    val setterMethod =
      (binderObject
        .getDeclaredMethods
        .toList
        .find(meth => meth.getName == "set")
        .get)
    var argsHolder: Array[Any] = null // this roundabout approach is to try and
    // make sure the value is boxed
    argsHolder = List(value).toArray
    setterMethod.invoke(null, argsHolder.asInstanceOf[Array[AnyRef]])

    interpret("val " + name + " = " + binderName + ".value")
  }

  /** Class to handle one member among all the members included
   *  in a single interpreter request.
   */
  private trait Handler {
    def member: Tree
    def usedNames: List[Name]
    val boundNames: List[Name]
    def valAndVarNames: List[Name]
    def defNames: List[Name]
    val importsWildcard: Boolean
    val importedNames: Seq[Name]
    val definesImplicit: Boolean
    def extraCodeToEvaluate(req: Request, code: PrintWriter): Unit
    def resultExtractionCode(req: Request, code: PrintWriter): Unit
  }

  /** Build a request from the user. <code>trees</code> is <code>line</code>
   *  after being parsed.
   */
  private def buildRequest(trees: List[Tree], line: String, lineName: String)(implicit ctx: Context): Request =
    new Request(line, lineName)

  /** One line of code submitted by the user for interpretation */
  private class Request(val line: String, val lineName: String)(implicit ctx: Context) {
    val trees = parse(line) match {
      case Some(ts) => ts
      case None => Nil
    }

    /** name to use for the object that will compute "line" */
    def objectName = lineName + INTERPRETER_WRAPPER_SUFFIX

    /** name of the object that retrieves the result from the above object */
    def resultObjectName = "RequestResult$" + objectName

    private val handlers: List[MemberHandler] = trees.flatMap(chooseHandler(_))

    /** all (public) names defined by these statements */
    val boundNames = (ListSet() ++ handlers.flatMap(_.boundNames)).toList

    /** list of names used by this expression */
    val usedNames: List[Name] = handlers.flatMap(_.usedNames)

    def myImportsCode = importsCode(Set.empty ++ usedNames)

    /** Code to append to objectName to access anything that
     *  the request binds.  */
    val accessPath = myImportsCode._3


    /** Code to access a variable with the specified name */
    def fullPath(vname: String): String =
      objectName + accessPath + ".`" + vname + "`"

    /** Code to access a variable with the specified name */
    def fullPath(vname: Name): String = fullPath(vname.toString)

    /** the line of code to compute */
    def toCompute = line

    /** generate the source code for the object that computes this request */
    def objectSourceCode: String =
      stringFrom { code =>
        // header for the wrapper object
        code.println("object " + objectName + " {")

        val (importsPreamble, importsTrailer, _) = myImportsCode

        code.print(importsPreamble)

        code.println(indentCode(toCompute))

        handlers.foreach(_.extraCodeToEvaluate(this,code))

        code.println(importsTrailer)

        //end the wrapper object
        code.println(";}")
      }

    /** Types of variables defined by this request.  They are computed
        after compilation of the main object */
    var typeOf: Map[Name, String] = _

    /** generate source code for the object that retrieves the result
        from objectSourceCode */
    def resultObjectSourceCode: String =
      stringFrom(code => {
        code.println("object " + resultObjectName)
        code.println("{ val result: String = {")
        code.println(objectName + accessPath + ";")  // evaluate the object, to make sure its constructor is run
        code.print("(\"\"")  // print an initial empty string, so later code can
                            // uniformly be: + morestuff
        handlers.foreach(_.resultExtractionCode(this, code))
        code.println("\n)}")
        code.println(";}")
      })


    /** Compile the object file.  Returns whether the compilation succeeded.
     *  If all goes well, the "types" map is computed. */
    def compile(): Boolean =
      compileSources(
        List(new SourceFile("<console>", objectSourceCode.toCharArray))) && {
        // extract and remember types
        this.typeOf = findTypes()
        compileSources(
          List(new SourceFile("<console>", resultObjectSourceCode.toCharArray)))
      }

    /** Dig the types of all bound variables out of the compiler run.
     *
     *  @param objRun ...
     *  @return       ...
     */
    def findTypes(): Map[Name, String] = {
      def valAndVarNames = handlers.flatMap(_.valAndVarNames)
      def defNames = handlers.flatMap(_.defNames)

      def getTypes(names: List[Name], nameMap: Name=>Name): Map[Name, String] = {
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
          val cleanedType = rawType.widenExpr

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

    /** load and run the code using reflection */
    def loadAndRun: (String, Boolean) = {
      val interpreterResultObject: Class[_] =
        Class.forName(resultObjectName, true, classLoader)
      val resultValMethod: java.lang.reflect.Method =
        interpreterResultObject.getMethod("result", null)
      try {
        (resultValMethod.invoke(interpreterResultObject, null).toString(),
             true)
      } catch {
        case e =>
          def caus(e: Throwable): Throwable =
            if (e.getCause eq null) e else caus(e.getCause)
          val orig = caus(e)
          (stringFrom(str => orig.printStackTrace(str)), false)
      }
    }

    /** Compute imports that allow definitions from previous
     *  requests to be visible in a new request.  Returns
     *  three pieces of related code:
     *
     *  1. An initial code fragment that should go before
     *  the code of the new request.
     *
     *  2. A code fragment that should go after the code
     *  of the new request.
     *
     *  3. An access path which can be traversed to access
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
      def reqsToUse: List[(Request, Handler)] = {
        /** Loop through a list of MemberHandlers and select
         *  which ones to keep.  'wanted' is the set of
         *  names that need to be imported, and
         *  'shadowed' is the list of names useless to import
         *  because a later request will re-import it anyway.
         */
        def select(reqs: List[(Request, Handler)], wanted: Set[Name]): List[(Request, Handler)] = {
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

      val code = new StringBuffer
      val trailingBraces = new StringBuffer
      val accessPath = new StringBuffer
      val impname = INTERPRETER_IMPORT_WRAPPER
      val currentImps = mutable.Set.empty[Name]

      // add code for a new object to hold some imports
      def addWrapper(): Unit = {
        code.append("object " + impname + "{\n")
        trailingBraces.append("}\n")
        accessPath.append("." + impname)
        currentImps.clear
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

        if (handler.member.isInstanceOf[Import])
          code.append(handler.member.toString + ";\n")

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
          code.append("import ")
          code.append(req.objectName + req.accessPath + ".`" + imv + "`;\n")
          currentImps += imv
        }
      }

      addWrapper() // Add one extra wrapper, to prevent warnings
      // in the frequent case of redefining
      // the value bound in the last interpreter
      // request.

      (code.toString, trailingBraces.toString, accessPath.toString)
    }

    // ------ Handlers ------------------------------------------

    private def chooseHandler(member: Tree): Option[MemberHandler] =
      member match {
        case member: DefDef =>
          Some(new DefHandler(member))
        case member: ValDef =>
          Some(new ValHandler(member))
        case member @ Assign(Ident(_), _) => Some(new AssignHandler(member))
        case member: ModuleDef => Some(new ModuleHandler(member))
        case member: TypeDef =>
          Some(if (member.isClassDef) new ClassHandler(member)
          else new TypeAliasHandler(member))
        case member: Import => Some(new ImportHandler(member))
        //      case DocDef(_, documented) => chooseHandler(documented)
        case member => Some(new GenericHandler(member))
      }

    /** A traverser that finds all mentioned identifiers, i.e. things
     *  that need to be imported.
     *  It might return extra names.
     */
    private class ImportVarsTraverser(definedVars: List[Name]) extends TreeTraverser {
      val importVars = new HashSet[Name]()

      override def traverse(ast: Tree)(implicit ctx: Context): Unit = {
        ast match {
          case Ident(name) => importVars += name
          case _ => traverseChildren(ast)
        }
      }
    }

    /** Class to handle one member among all the members included
     *  in a single interpreter request.
     */
    private sealed abstract class MemberHandler(val member: Tree) extends Handler {
      val usedNames: List[Name] = {
        val ivt = new ImportVarsTraverser(boundNames)
        ivt.traverse(member)
        ivt.importVars.toList
      }
      val boundNames: List[Name] = Nil
      def valAndVarNames: List[Name] = Nil
      def defNames: List[Name] = Nil
      val importsWildcard = false
      val importedNames: Seq[Name] = Nil
      val definesImplicit =
        member match {
          case tree: MemberDef => tree.mods.is(Flags.Implicit)
          case _ => false
        }

      def extraCodeToEvaluate(req: Request, code: PrintWriter) = {}
      def resultExtractionCode(req: Request, code: PrintWriter) = {}
    }

    private class GenericHandler(member: Tree) extends MemberHandler(member)

    private class ValHandler(member: ValDef) extends MemberHandler(member) {
      override val boundNames = List(member.name)
      override def valAndVarNames = boundNames

      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        val vname = member.name
        if (!member.mods.is(Flags.AccessFlags) &&
          !(isGeneratedVarName(vname.toString) &&
            req.typeOf(vname.encode) == "Unit")) {
          val prettyName = vname.decode
          code.print(" + \"" + prettyName + ": " +
            string2code(req.typeOf(vname)) +
            " = \" + " +
            " (if(" +
            req.fullPath(vname) +
            ".asInstanceOf[AnyRef] != null) " +
            " ((if(" +
            req.fullPath(vname) +
            ".toString().contains('\\n')) " +
            " \"\\n\" else \"\") + " +
            req.fullPath(vname) + ".toString() + \"\\n\") else \"null\\n\") ")
        }
      }
    }

    private class DefHandler(defDef: DefDef) extends MemberHandler(defDef) {
      override val boundNames = List(defDef.name)
      override def defNames = boundNames

      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        if (!defDef.mods.is(Flags.AccessFlags))
          code.print("+\"" + string2code(defDef.name.toString) + ": " +
            string2code(req.typeOf(defDef.name)) + "\\n\"")
      }
    }

    private class AssignHandler(member: Assign) extends MemberHandler(member) {
      val lhs = member.lhs.asInstanceOf[Ident] // an unfortunate limitation

      val helperName = newInternalVarName().toTermName
      override val valAndVarNames = List(helperName)

      override def extraCodeToEvaluate(req: Request, code: PrintWriter): Unit = {
        code.println("val " + helperName + " = " + member.lhs + ";")
      }

      /** Print out lhs instead of the generated varName */
      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        code.print(" + \"" + lhs + ": " +
          string2code(req.typeOf(helperName.encode)) +
          " = \" + " +
          string2code(req.fullPath(helperName))
          + " + \"\\n\"")
      }
    }

    private class ModuleHandler(module: ModuleDef) extends MemberHandler(module) {
      override val boundNames = List(module.name)

      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        code.println(" + \"defined module " +
          string2code(module.name.toString)
          + "\\n\"")
      }
    }

    private class ClassHandler(classdef: TypeDef)
      extends MemberHandler(classdef) {
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
      extends MemberHandler(typeDef) {
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

    private class ImportHandler(imp: Import) extends MemberHandler(imp) {
      override def resultExtractionCode(req: Request, code: PrintWriter): Unit = {
        code.println("+ \"" + imp.toString + "\\n\"")
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

  /** next result variable number to use */
  private var nextVarNameNo = 0

  /** allocate a fresh variable name */
  private def newVarName() = {
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
    stream.close
    stringWriter.toString
  }

  /** Truncate a string if it is longer than settings.maxPrintString */
  private def truncPrintString(str: String): String = {
    val maxpr = isettings.maxPrintString

    if (maxpr <= 0)
      return str

    if (str.length <= maxpr)
      return str

    val trailer = "..."
    if (maxpr >= trailer.length+1)
      return str.substring(0, maxpr-3) + trailer

    str.substring(0, maxpr)
  }

  /** Clean up a string for output */
  private def clean(str: String) =
    truncPrintString(Interpreter.stripWrapperGunk(str))

  /** Indent some code by the width of the scala> prompt.
   *  This way, compiler error messages read better.
   */
  def indentCode(code: String) = {
    val spaces = "       "

    stringFrom(str =>
      for (line <- code.lines) {
        str.print(spaces)
        str.print(line + "\n")
        str.flush()
      })
  }
}

/** Utility methods for the Interpreter. */
object Interpreter {
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
