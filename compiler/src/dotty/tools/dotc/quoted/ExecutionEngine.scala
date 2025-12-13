package dotty.tools.dotc
package quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.util.SourcePosition

/**
 * Execution engine for running Scala programs via TASTy interpretation.
 *
 * This engine finds the entry point (main method) of a compiled program
 * and executes it using the TastyBasedInterpreter.
 *
 * Usage:
 * {{{
 *   val engine = ExecutionEngine(ctx)
 *   val result = engine.execute(tree)
 *   println(result.output)
 * }}}
 */
class ExecutionEngine(using Context):

  /** Result of program execution */
  case class ExecutionResult(
    success: Boolean,
    output: String,
    returnValue: Option[Any],
    error: Option[Throwable]
  )

  /**
   * Execute a compiled program tree.
   *
   * @param tree The top-level tree (PackageDef or TypeDef) containing the program
   * @param mainClass The name of the main class/object (default: "Main")
   * @param mainMethod The name of the entry method (default: "main")
   * @return ExecutionResult containing output and any return value
   */
  def execute(
    tree: Tree,
    mainClass: String = "Main",
    mainMethod: String = "main"
  ): ExecutionResult =
    try
      val interpreter = createInterpreter(tree.srcPos)

      // Find the main method
      findMainMethod(tree, mainClass, mainMethod) match
        case Some(mainDef) =>
          // Execute the main method
          val args = Array[String]() // Empty args for now
          executeMainMethod(interpreter, mainDef, args)

          ExecutionResult(
            success = true,
            output = interpreter.getCapturedOutput,
            returnValue = None,
            error = None
          )

        case None =>
          // Try to find any object with a main method
          findAnyMainMethod(tree) match
            case Some(mainDef) =>
              val args = Array[String]()
              executeMainMethod(interpreter, mainDef, args)

              ExecutionResult(
                success = true,
                output = interpreter.getCapturedOutput,
                returnValue = None,
                error = None
              )

            case None =>
              ExecutionResult(
                success = false,
                output = "",
                returnValue = None,
                error = Some(new RuntimeException(
                  s"No main method found. Expected: object $mainClass { def $mainMethod(args: Array[String]): Unit = ... }"
                ))
              )

    catch
      case e: Interpreter.StopInterpretation =>
        ExecutionResult(
          success = false,
          output = "",
          returnValue = None,
          error = Some(new RuntimeException(e.msg.message))
        )
      case e: Throwable =>
        ExecutionResult(
          success = false,
          output = "",
          returnValue = None,
          error = Some(e)
        )

  /**
   * Execute an expression and return its value.
   *
   * @param tree The expression tree to evaluate
   * @return ExecutionResult containing the evaluated value
   */
  def evaluate(tree: Tree): ExecutionResult =
    try
      val interpreter = createInterpreter(tree.srcPos)
      val result = interpreter.interpret[Any](tree)

      ExecutionResult(
        success = true,
        output = interpreter.getCapturedOutput,
        returnValue = result,
        error = None
      )
    catch
      case e: Interpreter.StopInterpretation =>
        ExecutionResult(
          success = false,
          output = "",
          returnValue = None,
          error = Some(new RuntimeException(e.msg.message))
        )
      case e: Throwable =>
        ExecutionResult(
          success = false,
          output = "",
          returnValue = None,
          error = Some(e)
        )

  /** Create a TastyBasedInterpreter instance */
  private def createInterpreter(pos: SrcPos): TastyBasedInterpreter =
    val classLoader = getClass.getClassLoader
    new TastyBasedInterpreter(pos, classLoader)

  /** Find main method in specified class */
  private def findMainMethod(tree: Tree, mainClass: String, mainMethod: String): Option[DefDef] =
    tree match
      case PackageDef(pid, stats) =>
        stats.flatMap(findMainMethodInStat(_, mainClass, mainMethod)).headOption

      case tdef: TypeDef if tdef.name.toString == mainClass =>
        findMainMethodInClass(tdef, mainMethod)

      case _ => None

  /** Find main method in any object */
  private def findAnyMainMethod(tree: Tree): Option[DefDef] =
    tree match
      case PackageDef(pid, stats) =>
        stats.flatMap(findMainMethodInAnyStat).headOption

      case tdef: TypeDef =>
        findMainMethodInClass(tdef, "main")

      case _ => None

  /** Find main method in a statement */
  private def findMainMethodInStat(stat: Tree, mainClass: String, mainMethod: String): Option[DefDef] =
    stat match
      case tdef: TypeDef if tdef.name.toString == mainClass || tdef.name.toString == mainClass + "$" =>
        findMainMethodInClass(tdef, mainMethod)
      case _ => None

  /** Find main method in any statement */
  private def findMainMethodInAnyStat(stat: Tree): Option[DefDef] =
    stat match
      case tdef: TypeDef if tdef.symbol.is(Module) =>
        findMainMethodInClass(tdef, "main")
      case _ => None

  /** Find main method in a class/object definition */
  private def findMainMethodInClass(tdef: TypeDef, mainMethod: String): Option[DefDef] =
    tdef.rhs match
      case template: Template =>
        template.body.collectFirst {
          case ddef: DefDef if ddef.name.toString == mainMethod && isMainMethodSignature(ddef) =>
            ddef
        }
      case _ => None

  /** Check if a method has the standard main method signature */
  private def isMainMethodSignature(ddef: DefDef): Boolean =
    ddef.termParamss match
      case List(List(param)) =>
        // Should have one parameter of type Array[String]
        val paramType = param.tpt.tpe
        paramType.typeSymbol.fullName.toString.contains("Array")
      case List() =>
        // Also accept no-arg main for simpler programs
        true
      case _ => false

  /** Execute a main method */
  private def executeMainMethod(interpreter: TastyBasedInterpreter, mainDef: DefDef, args: Array[String]): Unit =
    interpreter.executeMainMethod(mainDef, args)

end ExecutionEngine

object ExecutionEngine:
  /** Convenience method to execute a program */
  def run(tree: Tree)(using Context): ExecutionEngine#ExecutionResult =
    val engine = new ExecutionEngine
    engine.execute(tree)

  /** Convenience method to evaluate an expression */
  def eval(tree: Tree)(using Context): ExecutionEngine#ExecutionResult =
    val engine = new ExecutionEngine
    engine.evaluate(tree)

