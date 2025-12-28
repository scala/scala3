package interpreter

import scala.collection.mutable

/**
 * Pure Scala interpreter for the browser AST.
 *
 * This interpreter can be cross-compiled to JavaScript via Scala.js.
 * It provides all core Scala functionality without JVM dependencies.
 */
class Interpreter {
  import Ast._
  import Pattern._

  // Output buffer for println/print
  private val outputBuffer = new StringBuilder()

  // Statistics
  private var nodeCount = 0
  private var callCount = 0

  // Environment type
  type Env = Map[String, Value]

  // Value representation
  sealed trait Value
  case class IntValue(v: Int) extends Value
  case class LongValue(v: Long) extends Value
  case class DoubleValue(v: Double) extends Value
  case class FloatValue(v: Float) extends Value
  case class BoolValue(v: Boolean) extends Value
  case class StringValue(v: String) extends Value
  case class CharValue(v: Char) extends Value
  case object UnitValue extends Value
  case object NullValue extends Value
  case class ListValue(elements: List[Value]) extends Value
  case class OptionValue(value: Option[Value]) extends Value
  case class TupleValue(elements: List[Value]) extends Value
  case class ClosureValue(params: List[String], body: Ast, env: Env) extends Value
  case class DefValue(params: List[String], body: Ast, defEnv: Env) extends Value
  case class VarCell(var value: Value) extends Value
  case class ExceptionValue(tpe: String, message: String) extends Value

  // Interpreter exception for non-local returns
  private class ReturnException(val value: Value) extends Exception
  private class ThrowException(val exc: ExceptionValue) extends Exception(exc.message)

  /**
   * Interpret an AST and return the result.
   */
  def interpret(ast: Ast): InterpreterResult = {
    outputBuffer.clear()
    nodeCount = 0
    callCount = 0

    try {
      val result = eval(ast, Map.empty)
      InterpreterResult(
        success = true,
        output = outputBuffer.toString,
        result = Some(valueToString(result)),
        error = None,
        stats = Stats(nodeCount, callCount)
      )
    } catch {
      case e: ThrowException =>
        InterpreterResult(
          success = false,
          output = outputBuffer.toString,
          result = None,
          error = Some(s"${e.exc.tpe}: ${e.exc.message}"),
          stats = Stats(nodeCount, callCount)
        )
      case e: Exception =>
        InterpreterResult(
          success = false,
          output = outputBuffer.toString,
          result = None,
          error = Some(e.getMessage),
          stats = Stats(nodeCount, callCount)
        )
    }
  }

  /**
   * Get captured output.
   */
  def getOutput: String = outputBuffer.toString

  /**
   * Clear output buffer.
   */
  def clearOutput(): Unit = outputBuffer.clear()

  /**
   * Evaluate an AST node.
   */
  private def eval(ast: Ast, env: Env): Value = {
    nodeCount += 1

    ast match {
      // Literals
      case IntLit(v) => IntValue(v)
      case LongLit(v) => LongValue(v)
      case DoubleLit(v) => DoubleValue(v)
      case FloatLit(v) => FloatValue(v)
      case BoolLit(v) => BoolValue(v)
      case StringLit(v) => StringValue(v)
      case CharLit(v) => CharValue(v)
      case UnitLit => UnitValue
      case NullLit => NullValue

      // References
      case Ident(name) =>
        name match {
          case "None" => OptionValue(None)
          case "Nil" => ListValue(Nil)
          case _ => env.get(name) match {
            case Some(VarCell(v)) => v
            case Some(v) => v
            case None => throw new RuntimeException(s"Undefined variable: $name")
          }
        }

      case Select(receiver, name) =>
        val recv = eval(receiver, env)
        getProperty(recv, name)

      // Definitions
      case ValDef(name, rhs, mutable) =>
        val value = eval(rhs, env)
        if (mutable) VarCell(value) else value

      case DefDef(name, params, body) =>
        DefValue(params, body, env)

      // Control flow
      case Block(stats, expr) =>
        var localEnv = env
        for (stat <- stats) {
          stat match {
            case ValDef(name, rhs, mutable) =>
              val value = eval(rhs, localEnv)
              localEnv = localEnv + (name -> (if (mutable) VarCell(value) else value))
            case DefDef(name, params, body) =>
              localEnv = localEnv + (name -> DefValue(params, body, localEnv))
            case _ =>
              eval(stat, localEnv)
          }
        }
        eval(expr, localEnv)

      case If(cond, thenp, elsep) =>
        val condVal = eval(cond, env)
        if (toBool(condVal)) eval(thenp, env) else eval(elsep, env)

      case While(cond, body) =>
        while (toBool(eval(cond, env))) {
          eval(body, env)
        }
        UnitValue

      case Match(selector, cases) =>
        val scrutinee = eval(selector, env)
        evalMatch(scrutinee, cases, env)

      case Try(block, catches, finalizer) =>
        try {
          val result = eval(block, env)
          finalizer.foreach(f => eval(f, env))
          result
        } catch {
          case e: ThrowException =>
            val excValue = e.exc
            catches.find(c => matchesPattern(excValue, c.pattern)) match {
              case Some(caseDef) =>
                val bindings = extractBindings(excValue, caseDef.pattern)
                val result = eval(caseDef.body, env ++ bindings)
                finalizer.foreach(f => eval(f, env))
                result
              case None =>
                finalizer.foreach(f => eval(f, env))
                throw e
            }
        }

      case Return(expr) =>
        throw new ReturnException(eval(expr, env))

      case Throw(expr) =>
        val value = eval(expr, env)
        value match {
          case exc: ExceptionValue => throw new ThrowException(exc)
          case StringValue(msg) => throw new ThrowException(ExceptionValue("RuntimeException", msg))
          case _ => throw new ThrowException(ExceptionValue("RuntimeException", valueToString(value)))
        }

      // Operations
      case BinaryOp(op, lhs, rhs) =>
        evalBinaryOp(op, eval(lhs, env), eval(rhs, env))

      case UnaryOp(op, arg) =>
        evalUnaryOp(op, eval(arg, env))

      case Apply(fn, args) =>
        callCount += 1
        evalApply(fn, args, env)

      case New(className, args) =>
        val argVals = args.map(a => eval(a, env))
        createInstance(className, argVals)

      case Assign(name, rhs) =>
        env.get(name) match {
          case Some(cell: VarCell) =>
            cell.value = eval(rhs, env)
            UnitValue
          case _ =>
            throw new RuntimeException(s"Cannot assign to $name")
        }

      // Functions
      case Lambda(params, body) =>
        ClosureValue(params, body, env)
    }
  }

  /**
   * Evaluate a function application.
   */
  private def evalApply(fn: Ast, args: List[Ast], env: Env): Value = {
    fn match {
      case Ident(name) =>
        evalFunctionCall(name, args.map(a => eval(a, env)), env)

      case Select(receiver, methodName) =>
        val recv = eval(receiver, env)
        val argVals = args.map(a => eval(a, env))
        callMethod(recv, methodName, argVals, env)

      case _ =>
        val fnVal = eval(fn, env)
        val argVals = args.map(a => eval(a, env))
        applyClosure(fnVal, argVals, env)
    }
  }

  /**
   * Evaluate a named function call.
   */
  private def evalFunctionCall(name: String, args: List[Value], env: Env): Value = {
    name match {
      // I/O
      case "println" =>
        val msg = if (args.isEmpty) "" else valueToString(args.head)
        outputBuffer.append(msg).append("\n")
        UnitValue
      case "print" =>
        outputBuffer.append(valueToString(args.head))
        UnitValue

      // Constructors
      case "List" => ListValue(args)
      case "Some" => OptionValue(Some(args.head))
      case "None" => OptionValue(None)
      case "Tuple" | "Tuple2" | "Tuple3" | "Tuple4" | "Tuple5" => TupleValue(args)

      // Assertions
      case "require" =>
        if (!toBool(args.head)) {
          val msg = if (args.size > 1) valueToString(args(1)) else "requirement failed"
          throw new ThrowException(ExceptionValue("IllegalArgumentException", msg))
        }
        UnitValue
      case "assert" =>
        if (!toBool(args.head)) {
          val msg = if (args.size > 1) valueToString(args(1)) else "assertion failed"
          throw new ThrowException(ExceptionValue("AssertionError", msg))
        }
        UnitValue

      // Local function call
      case _ =>
        env.get(name) match {
          case Some(DefValue(params, body, defEnv)) =>
            val localEnv = defEnv ++ params.zip(args).toMap + (name -> DefValue(params, body, defEnv))
            eval(body, localEnv)
          case Some(closure: ClosureValue) =>
            applyClosure(closure, args, env)
          case Some(other) =>
            other // Return the value itself
          case None =>
            throw new RuntimeException(s"Unknown function: $name")
        }
    }
  }

  /**
   * Call a method on a value.
   */
  private def callMethod(receiver: Value, method: String, args: List[Value], env: Env): Value = {
    receiver match {
      // Null/None handling
      case NullValue | OptionValue(None) =>
        method match {
          case "isEmpty" => BoolValue(true)
          case "isDefined" | "nonEmpty" => BoolValue(false)
          case "getOrElse" => applyThunk(args.head, env)
          case "orElse" => applyThunk(args.head, env)
          case "toString" => StringValue(if (receiver == NullValue) "null" else "None")
          case _ => throw new ThrowException(ExceptionValue("NullPointerException", s"Cannot call $method on null/None"))
        }

      // String methods
      case StringValue(s) =>
        method match {
          case "length" => IntValue(s.length)
          case "charAt" => CharValue(s.charAt(toInt(args.head)))
          case "substring" =>
            if (args.size == 1) StringValue(s.substring(toInt(args.head)))
            else StringValue(s.substring(toInt(args(0)), toInt(args(1))))
          case "toUpperCase" => StringValue(s.toUpperCase)
          case "toLowerCase" => StringValue(s.toLowerCase)
          case "trim" => StringValue(s.trim)
          case "isEmpty" => BoolValue(s.isEmpty)
          case "nonEmpty" => BoolValue(s.nonEmpty)
          case "contains" => BoolValue(s.contains(toString(args.head)))
          case "startsWith" => BoolValue(s.startsWith(toString(args.head)))
          case "endsWith" => BoolValue(s.endsWith(toString(args.head)))
          case "indexOf" => IntValue(s.indexOf(toString(args.head)))
          case "replace" => StringValue(s.replace(toString(args(0)), toString(args(1))))
          case "split" => ListValue(s.split(toString(args.head)).map(StringValue(_)).toList)
          case "reverse" => StringValue(s.reverse)
          case "toInt" => IntValue(s.toInt)
          case "toDouble" => DoubleValue(s.toDouble)
          case "toString" => StringValue(s)
          case "+" => StringValue(s + valueToString(args.head))
          case _ => throw new RuntimeException(s"Unknown String method: $method")
        }

      // Number methods
      case IntValue(n) =>
        method match {
          case "toString" => StringValue(n.toString)
          case "toInt" => IntValue(n)
          case "toLong" => LongValue(n.toLong)
          case "toDouble" => DoubleValue(n.toDouble)
          case "abs" => IntValue(math.abs(n))
          case "max" => IntValue(math.max(n, toInt(args.head)))
          case "min" => IntValue(math.min(n, toInt(args.head)))
          case _ => throw new RuntimeException(s"Unknown Int method: $method")
        }

      case DoubleValue(n) =>
        method match {
          case "toString" => StringValue(n.toString)
          case "toInt" => IntValue(n.toInt)
          case "toDouble" => DoubleValue(n)
          case "abs" => DoubleValue(math.abs(n))
          case "max" => DoubleValue(math.max(n, toDouble(args.head)))
          case "min" => DoubleValue(math.min(n, toDouble(args.head)))
          case _ => throw new RuntimeException(s"Unknown Double method: $method")
        }

      // List methods
      case ListValue(elements) =>
        method match {
          case "head" =>
            if (elements.isEmpty) throw new ThrowException(ExceptionValue("NoSuchElementException", "head of empty list"))
            elements.head
          case "tail" =>
            if (elements.isEmpty) throw new ThrowException(ExceptionValue("UnsupportedOperationException", "tail of empty list"))
            ListValue(elements.tail)
          case "last" =>
            if (elements.isEmpty) throw new ThrowException(ExceptionValue("NoSuchElementException", "last of empty list"))
            elements.last
          case "init" => ListValue(elements.init)
          case "isEmpty" => BoolValue(elements.isEmpty)
          case "nonEmpty" => BoolValue(elements.nonEmpty)
          case "size" | "length" => IntValue(elements.size)
          case "reverse" => ListValue(elements.reverse)
          case "apply" => elements(toInt(args.head))
          case "take" => ListValue(elements.take(toInt(args.head)))
          case "drop" => ListValue(elements.drop(toInt(args.head)))
          case "contains" => BoolValue(elements.contains(args.head))
          case "indexOf" => IntValue(elements.indexOf(args.head))
          case "distinct" => ListValue(elements.distinct)
          case "sum" => elements.foldLeft[Value](IntValue(0))((a, b) => evalBinaryOp("+", a, b))
          case "product" => elements.foldLeft[Value](IntValue(1))((a, b) => evalBinaryOp("*", a, b))
          case "min" => elements.reduce((a, b) => if (compare(a, b) < 0) a else b)
          case "max" => elements.reduce((a, b) => if (compare(a, b) > 0) a else b)
          case "mkString" =>
            val strings = elements.map(valueToString)
            args.size match {
              case 0 => StringValue(strings.mkString)
              case 1 => StringValue(strings.mkString(toString(args.head)))
              case _ => StringValue(strings.mkString(toString(args(0)), toString(args(1)), toString(args(2))))
            }
          case "toString" => StringValue(s"List(${elements.map(valueToString).mkString(", ")})")
          case "toList" => receiver
          case "zip" =>
            val other = toList(args.head)
            ListValue(elements.zip(other).map { case (a, b) => TupleValue(List(a, b)) })
          case "zipWithIndex" =>
            ListValue(elements.zipWithIndex.map { case (a, i) => TupleValue(List(a, IntValue(i))) })
          case "map" =>
            ListValue(elements.map(x => applyClosure(args.head, List(x), env)))
          case "flatMap" =>
            ListValue(elements.flatMap(x => toList(applyClosure(args.head, List(x), env))))
          case "filter" =>
            ListValue(elements.filter(x => toBool(applyClosure(args.head, List(x), env))))
          case "filterNot" =>
            ListValue(elements.filterNot(x => toBool(applyClosure(args.head, List(x), env))))
          case "find" =>
            elements.find(x => toBool(applyClosure(args.head, List(x), env))) match {
              case Some(v) => OptionValue(Some(v))
              case None => OptionValue(None)
            }
          case "exists" =>
            BoolValue(elements.exists(x => toBool(applyClosure(args.head, List(x), env))))
          case "forall" =>
            BoolValue(elements.forall(x => toBool(applyClosure(args.head, List(x), env))))
          case "count" =>
            IntValue(elements.count(x => toBool(applyClosure(args.head, List(x), env))))
          case "foreach" =>
            elements.foreach(x => applyClosure(args.head, List(x), env))
            UnitValue
          case "foldLeft" =>
            elements.foldLeft(args(0))((acc, x) => applyClosure(args(1), List(acc, x), env))
          case "foldRight" =>
            elements.foldRight(args(0))((x, acc) => applyClosure(args(1), List(x, acc), env))
          case "reduce" =>
            if (elements.isEmpty) throw new ThrowException(ExceptionValue("UnsupportedOperationException", "empty.reduce"))
            elements.reduce((a, b) => applyClosure(args.head, List(a, b), env))
          case "::" | "$colon$colon" =>
            ListValue(args.head :: elements)
          case "++" | "$plus$plus" =>
            ListValue(elements ++ toList(args.head))
          case "sorted" =>
            ListValue(elements.sortWith((a, b) => compare(a, b) < 0))
          case _ => throw new RuntimeException(s"Unknown List method: $method")
        }

      // Option methods
      case OptionValue(opt) =>
        method match {
          case "get" => opt.getOrElse(throw new ThrowException(ExceptionValue("NoSuchElementException", "None.get")))
          case "getOrElse" => opt.getOrElse(applyThunk(args.head, env))
          case "orElse" => if (opt.isDefined) receiver else applyThunk(args.head, env)
          case "isEmpty" => BoolValue(opt.isEmpty)
          case "isDefined" | "nonEmpty" => BoolValue(opt.isDefined)
          case "map" =>
            OptionValue(opt.map(x => applyClosure(args.head, List(x), env)))
          case "flatMap" =>
            opt match {
              case Some(v) => applyClosure(args.head, List(v), env)
              case None => OptionValue(None)
            }
          case "filter" =>
            OptionValue(opt.filter(x => toBool(applyClosure(args.head, List(x), env))))
          case "exists" =>
            BoolValue(opt.exists(x => toBool(applyClosure(args.head, List(x), env))))
          case "forall" =>
            BoolValue(opt.forall(x => toBool(applyClosure(args.head, List(x), env))))
          case "fold" =>
            opt match {
              case Some(v) => applyClosure(args(1), List(v), env)
              case None => applyThunk(args(0), env)
            }
          case "toList" => ListValue(opt.toList)
          case "toString" => StringValue(opt.map(v => s"Some(${valueToString(v)})").getOrElse("None"))
          case _ => throw new RuntimeException(s"Unknown Option method: $method")
        }

      // Tuple methods
      case TupleValue(elements) =>
        method match {
          case "_1" => elements(0)
          case "_2" => elements(1)
          case "_3" => elements(2)
          case "_4" => elements(3)
          case "_5" => elements(4)
          case "toString" => StringValue(s"(${elements.map(valueToString).mkString(", ")})")
          case _ => throw new RuntimeException(s"Unknown Tuple method: $method")
        }

      // Exception methods
      case ExceptionValue(tpe, msg) =>
        method match {
          case "getMessage" | "message" => StringValue(msg)
          case "toString" => StringValue(s"$tpe: $msg")
          case _ => throw new RuntimeException(s"Unknown Exception method: $method")
        }

      // Generic methods
      case _ =>
        method match {
          case "toString" => StringValue(valueToString(receiver))
          case "hashCode" => IntValue(receiver.hashCode)
          case "equals" => BoolValue(valuesEqual(receiver, args.head))
          case _ => throw new RuntimeException(s"Unknown method: $method on ${receiver.getClass.getSimpleName}")
        }
    }
  }

  /**
   * Apply a closure or thunk.
   */
  private def applyClosure(fnVal: Value, args: List[Value], env: Env): Value = {
    fnVal match {
      case ClosureValue(params, body, closureEnv) =>
        val localEnv = closureEnv ++ params.zip(args).toMap
        eval(body, localEnv)
      case DefValue(params, body, defEnv) =>
        val localEnv = defEnv ++ params.zip(args).toMap
        eval(body, localEnv)
      case _ =>
        throw new RuntimeException(s"Cannot apply non-function: ${fnVal.getClass.getSimpleName}")
    }
  }

  /**
   * Apply a thunk (0-arg closure).
   */
  private def applyThunk(fnVal: Value, env: Env): Value = {
    fnVal match {
      case ClosureValue(_, body, closureEnv) => eval(body, closureEnv)
      case other => other // Non-closure is returned as-is
    }
  }

  /**
   * Get a property from a value.
   */
  private def getProperty(receiver: Value, name: String): Value = {
    receiver match {
      case TupleValue(elements) =>
        name match {
          case "_1" => elements(0)
          case "_2" => elements(1)
          case "_3" => elements(2)
          case "_4" => elements(3)
          case "_5" => elements(4)
          case _ => throw new RuntimeException(s"Unknown tuple property: $name")
        }
      case ListValue(elements) =>
        name match {
          case "head" => elements.head
          case "tail" => ListValue(elements.tail)
          case "length" | "size" => IntValue(elements.size)
          case "isEmpty" => BoolValue(elements.isEmpty)
          case "nonEmpty" => BoolValue(elements.nonEmpty)
          case _ => throw new RuntimeException(s"Unknown List property: $name")
        }
      case StringValue(s) =>
        name match {
          case "length" => IntValue(s.length)
          case "isEmpty" => BoolValue(s.isEmpty)
          case "nonEmpty" => BoolValue(s.nonEmpty)
          case _ => throw new RuntimeException(s"Unknown String property: $name")
        }
      case OptionValue(opt) =>
        name match {
          case "isEmpty" => BoolValue(opt.isEmpty)
          case "isDefined" | "nonEmpty" => BoolValue(opt.isDefined)
          case "get" => opt.getOrElse(throw new ThrowException(ExceptionValue("NoSuchElementException", "None.get")))
          case _ => throw new RuntimeException(s"Unknown Option property: $name")
        }
      case ExceptionValue(_, msg) =>
        name match {
          case "message" | "getMessage" => StringValue(msg)
          case _ => throw new RuntimeException(s"Unknown Exception property: $name")
        }
      case _ =>
        throw new RuntimeException(s"Cannot access property $name on ${receiver.getClass.getSimpleName}")
    }
  }

  /**
   * Create a new instance.
   */
  private def createInstance(className: String, args: List[Value]): Value = {
    className match {
      case "RuntimeException" | "Exception" =>
        ExceptionValue(className, if (args.isEmpty) "" else valueToString(args.head))
      case "IllegalArgumentException" =>
        ExceptionValue(className, if (args.isEmpty) "" else valueToString(args.head))
      case "Some" => OptionValue(Some(args.head))
      case "Tuple2" => TupleValue(args.take(2))
      case "Tuple3" => TupleValue(args.take(3))
      case "Tuple4" => TupleValue(args.take(4))
      case "Tuple5" => TupleValue(args.take(5))
      case _ => throw new RuntimeException(s"Cannot instantiate: $className")
    }
  }

  /**
   * Evaluate a binary operation.
   */
  private def evalBinaryOp(op: String, lhs: Value, rhs: Value): Value = {
    op match {
      case "+" =>
        (lhs, rhs) match {
          case (StringValue(a), b) => StringValue(a + valueToString(b))
          case (a, StringValue(b)) => StringValue(valueToString(a) + b)
          case (IntValue(a), IntValue(b)) => IntValue(a + b)
          case (LongValue(a), LongValue(b)) => LongValue(a + b)
          case (DoubleValue(a), DoubleValue(b)) => DoubleValue(a + b)
          case (IntValue(a), DoubleValue(b)) => DoubleValue(a + b)
          case (DoubleValue(a), IntValue(b)) => DoubleValue(a + b)
          case _ => throw new RuntimeException(s"Cannot add ${lhs.getClass.getSimpleName} and ${rhs.getClass.getSimpleName}")
        }
      case "-" =>
        (lhs, rhs) match {
          case (IntValue(a), IntValue(b)) => IntValue(a - b)
          case (LongValue(a), LongValue(b)) => LongValue(a - b)
          case (DoubleValue(a), DoubleValue(b)) => DoubleValue(a - b)
          case (IntValue(a), DoubleValue(b)) => DoubleValue(a - b)
          case (DoubleValue(a), IntValue(b)) => DoubleValue(a - b)
          case _ => throw new RuntimeException(s"Cannot subtract")
        }
      case "*" =>
        (lhs, rhs) match {
          case (IntValue(a), IntValue(b)) => IntValue(a * b)
          case (LongValue(a), LongValue(b)) => LongValue(a * b)
          case (DoubleValue(a), DoubleValue(b)) => DoubleValue(a * b)
          case (IntValue(a), DoubleValue(b)) => DoubleValue(a * b)
          case (DoubleValue(a), IntValue(b)) => DoubleValue(a * b)
          case _ => throw new RuntimeException(s"Cannot multiply")
        }
      case "/" =>
        (lhs, rhs) match {
          case (IntValue(a), IntValue(b)) => IntValue(a / b)
          case (LongValue(a), LongValue(b)) => LongValue(a / b)
          case (DoubleValue(a), DoubleValue(b)) => DoubleValue(a / b)
          case (IntValue(a), DoubleValue(b)) => DoubleValue(a / b)
          case (DoubleValue(a), IntValue(b)) => DoubleValue(a / b)
          case _ => throw new RuntimeException(s"Cannot divide")
        }
      case "%" =>
        (lhs, rhs) match {
          case (IntValue(a), IntValue(b)) => IntValue(a % b)
          case (LongValue(a), LongValue(b)) => LongValue(a % b)
          case _ => throw new RuntimeException(s"Cannot modulo")
        }
      case "<" => BoolValue(compare(lhs, rhs) < 0)
      case ">" => BoolValue(compare(lhs, rhs) > 0)
      case "<=" => BoolValue(compare(lhs, rhs) <= 0)
      case ">=" => BoolValue(compare(lhs, rhs) >= 0)
      case "==" => BoolValue(valuesEqual(lhs, rhs))
      case "!=" => BoolValue(!valuesEqual(lhs, rhs))
      case "&&" => BoolValue(toBool(lhs) && toBool(rhs))
      case "||" => BoolValue(toBool(lhs) || toBool(rhs))
      case "::" =>
        rhs match {
          case ListValue(elements) => ListValue(lhs :: elements)
          case _ => throw new RuntimeException(s"Cannot prepend to non-list")
        }
      case _ => throw new RuntimeException(s"Unknown binary operator: $op")
    }
  }

  /**
   * Evaluate a unary operation.
   */
  private def evalUnaryOp(op: String, arg: Value): Value = {
    op match {
      case "-" =>
        arg match {
          case IntValue(n) => IntValue(-n)
          case LongValue(n) => LongValue(-n)
          case DoubleValue(n) => DoubleValue(-n)
          case FloatValue(n) => FloatValue(-n)
          case _ => throw new RuntimeException(s"Cannot negate")
        }
      case "!" =>
        BoolValue(!toBool(arg))
      case _ => throw new RuntimeException(s"Unknown unary operator: $op")
    }
  }

  /**
   * Evaluate pattern matching.
   */
  private def evalMatch(scrutinee: Value, cases: List[CaseDef], env: Env): Value = {
    for (caseDef <- cases) {
      if (matchesPattern(scrutinee, caseDef.pattern)) {
        val bindings = extractBindings(scrutinee, caseDef.pattern)
        val newEnv = env ++ bindings
        // Check guard
        if (caseDef.guard.forall(g => toBool(eval(g, newEnv)))) {
          return eval(caseDef.body, newEnv)
        }
      }
    }
    throw new ThrowException(ExceptionValue("MatchError", s"No case matched: ${valueToString(scrutinee)}"))
  }

  /**
   * Check if a value matches a pattern.
   */
  private def matchesPattern(value: Value, pattern: Pattern): Boolean = {
    pattern match {
      case Wildcard => true
      case Bind(_, inner) => inner.forall(p => matchesPattern(value, p))
      case Pattern.Literal(lit) =>
        lit match {
          case i: Int => value == IntValue(i)
          case l: Long => value == LongValue(l)
          case d: Double => value == DoubleValue(d)
          case b: Boolean => value == BoolValue(b)
          case s: String => value == StringValue(s)
          case _ => false
        }
      case Typed(tpe, inner) =>
        checkType(value, tpe) && inner.forall(p => matchesPattern(value, p))
      case Unapply(className, patterns) =>
        matchUnapply(value, className, patterns)
      case Alternative(alts) =>
        alts.exists(p => matchesPattern(value, p))
    }
  }

  /**
   * Check if a value matches a type.
   */
  private def checkType(value: Value, tpe: String): Boolean = {
    tpe match {
      case "Int" => value.isInstanceOf[IntValue]
      case "Long" => value.isInstanceOf[LongValue]
      case "Double" => value.isInstanceOf[DoubleValue]
      case "Float" => value.isInstanceOf[FloatValue]
      case "Boolean" => value.isInstanceOf[BoolValue]
      case "String" => value.isInstanceOf[StringValue]
      case "Char" => value.isInstanceOf[CharValue]
      case "List" => value.isInstanceOf[ListValue]
      case "Option" => value.isInstanceOf[OptionValue]
      case "Some" => value match { case OptionValue(Some(_)) => true; case _ => false }
      case "None" => value match { case OptionValue(None) => true; case _ => false }
      case "Throwable" | "Exception" | "RuntimeException" => value.isInstanceOf[ExceptionValue]
      case _ => true
    }
  }

  /**
   * Match an unapply pattern.
   */
  private def matchUnapply(value: Value, className: String, patterns: List[Pattern]): Boolean = {
    className match {
      case "Some" =>
        value match {
          case OptionValue(Some(v)) =>
            patterns.isEmpty || matchesPattern(v, patterns.head)
          case _ => false
        }
      case "None" =>
        value match {
          case OptionValue(None) => true
          case NullValue => true
          case _ => false
        }
      case "::" | "Cons" =>
        value match {
          case ListValue(h :: t) if patterns.size >= 2 =>
            matchesPattern(h, patterns(0)) && matchesPattern(ListValue(t), patterns(1))
          case _ => false
        }
      case "Nil" =>
        value match {
          case ListValue(Nil) => true
          case _ => false
        }
      case "Tuple2" =>
        value match {
          case TupleValue(List(a, b)) if patterns.size == 2 =>
            matchesPattern(a, patterns(0)) && matchesPattern(b, patterns(1))
          case _ => false
        }
      case "Tuple3" =>
        value match {
          case TupleValue(List(a, b, c)) if patterns.size == 3 =>
            matchesPattern(a, patterns(0)) && matchesPattern(b, patterns(1)) && matchesPattern(c, patterns(2))
          case _ => false
        }
      case _ => false
    }
  }

  /**
   * Extract bindings from a pattern match.
   */
  private def extractBindings(value: Value, pattern: Pattern): Map[String, Value] = {
    pattern match {
      case Wildcard => Map.empty
      case Bind(name, inner) =>
        val innerBindings = inner.map(p => extractBindings(value, p)).getOrElse(Map.empty)
        innerBindings + (name -> value)
      case Pattern.Literal(_) => Map.empty
      case Typed(_, inner) =>
        inner.map(p => extractBindings(value, p)).getOrElse(Map.empty)
      case Unapply(className, patterns) =>
        extractUnapplyBindings(value, className, patterns)
      case Alternative(alts) =>
        alts.find(p => matchesPattern(value, p))
          .map(p => extractBindings(value, p))
          .getOrElse(Map.empty)
    }
  }

  /**
   * Extract bindings from an unapply pattern.
   */
  private def extractUnapplyBindings(value: Value, className: String, patterns: List[Pattern]): Map[String, Value] = {
    className match {
      case "Some" =>
        value match {
          case OptionValue(Some(v)) if patterns.nonEmpty =>
            extractBindings(v, patterns.head)
          case _ => Map.empty
        }
      case "::" | "Cons" =>
        value match {
          case ListValue(h :: t) if patterns.size >= 2 =>
            extractBindings(h, patterns(0)) ++ extractBindings(ListValue(t), patterns(1))
          case _ => Map.empty
        }
      case "Tuple2" =>
        value match {
          case TupleValue(List(a, b)) if patterns.size == 2 =>
            extractBindings(a, patterns(0)) ++ extractBindings(b, patterns(1))
          case _ => Map.empty
        }
      case "Tuple3" =>
        value match {
          case TupleValue(List(a, b, c)) if patterns.size == 3 =>
            extractBindings(a, patterns(0)) ++ extractBindings(b, patterns(1)) ++ extractBindings(c, patterns(2))
          case _ => Map.empty
        }
      case _ => Map.empty
    }
  }

  // Conversion helpers
  private def toBool(v: Value): Boolean = v match {
    case BoolValue(b) => b
    case _ => throw new RuntimeException(s"Expected Boolean, got ${v.getClass.getSimpleName}")
  }

  private def toInt(v: Value): Int = v match {
    case IntValue(n) => n
    case LongValue(n) => n.toInt
    case DoubleValue(n) => n.toInt
    case _ => throw new RuntimeException(s"Expected number, got ${v.getClass.getSimpleName}")
  }

  private def toDouble(v: Value): Double = v match {
    case IntValue(n) => n.toDouble
    case LongValue(n) => n.toDouble
    case DoubleValue(n) => n
    case FloatValue(n) => n.toDouble
    case _ => throw new RuntimeException(s"Expected number")
  }

  private def toString(v: Value): String = v match {
    case StringValue(s) => s
    case CharValue(c) => c.toString
    case _ => valueToString(v)
  }

  private def toList(v: Value): List[Value] = v match {
    case ListValue(elements) => elements
    case _ => throw new RuntimeException(s"Expected List")
  }

  private def compare(a: Value, b: Value): Int = {
    (a, b) match {
      case (IntValue(x), IntValue(y)) => x.compareTo(y)
      case (LongValue(x), LongValue(y)) => x.compareTo(y)
      case (DoubleValue(x), DoubleValue(y)) => x.compareTo(y)
      case (StringValue(x), StringValue(y)) => x.compareTo(y)
      case (IntValue(x), DoubleValue(y)) => x.toDouble.compareTo(y)
      case (DoubleValue(x), IntValue(y)) => x.compareTo(y.toDouble)
      case _ => 0
    }
  }

  private def valuesEqual(a: Value, b: Value): Boolean = {
    (a, b) match {
      case (IntValue(x), IntValue(y)) => x == y
      case (LongValue(x), LongValue(y)) => x == y
      case (DoubleValue(x), DoubleValue(y)) => x == y
      case (BoolValue(x), BoolValue(y)) => x == y
      case (StringValue(x), StringValue(y)) => x == y
      case (ListValue(xs), ListValue(ys)) =>
        xs.size == ys.size && xs.zip(ys).forall { case (a, b) => valuesEqual(a, b) }
      case (OptionValue(x), OptionValue(y)) =>
        (x, y) match {
          case (Some(a), Some(b)) => valuesEqual(a, b)
          case (None, None) => true
          case _ => false
        }
      case (TupleValue(xs), TupleValue(ys)) =>
        xs.size == ys.size && xs.zip(ys).forall { case (a, b) => valuesEqual(a, b) }
      case (UnitValue, UnitValue) => true
      case (NullValue, NullValue) => true
      case _ => a == b
    }
  }

  /**
   * Convert a value to string.
   */
  def valueToString(v: Value): String = v match {
    case IntValue(n) => n.toString
    case LongValue(n) => n.toString
    case DoubleValue(n) => n.toString
    case FloatValue(n) => n.toString
    case BoolValue(b) => b.toString
    case StringValue(s) => s
    case CharValue(c) => c.toString
    case UnitValue => "()"
    case NullValue => "null"
    case ListValue(elements) => s"List(${elements.map(valueToString).mkString(", ")})"
    case OptionValue(Some(v)) => s"Some(${valueToString(v)})"
    case OptionValue(None) => "None"
    case TupleValue(elements) => s"(${elements.map(valueToString).mkString(", ")})"
    case ClosureValue(params, _, _) => s"<closure(${params.mkString(", ")})>"
    case DefValue(params, _, _) => s"<def(${params.mkString(", ")})>"
    case VarCell(value) => valueToString(value)
    case ExceptionValue(tpe, msg) => s"$tpe: $msg"
  }
}

/**
 * Result of interpretation.
 */
case class InterpreterResult(
  success: Boolean,
  output: String,
  result: Option[String],
  error: Option[String],
  stats: Stats
)

case class Stats(nodes: Int, calls: Int)

