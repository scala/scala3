package scala.tasty.interpreter
package pure

import scala.quoted.*
import scala.collection.mutable

/**
 * Pure TASTy interpreter - interprets ALL code from TASTy trees without JVM reflection.
 *
 * This is the foundation for cross-platform macro execution (Scala-Native, Scala-JS).
 */
class PureTastyInterpreter[Q <: Quotes & Singleton](using q0: Q) extends TreeInterpreter[Q] {
  import q.reflect.*

  // All references are represented by themselves and values are boxed
  type AbstractAny = Any

  val tastyLoader = new TastyLoader(using q)

  // For non-local returns
  private class ReturnException(val value: AbstractAny) extends Exception

  // Exception wrapper for interpreted throws
  private class InterpretedException(val underlying: Throwable) extends RuntimeException(underlying)

  //==========================================================================
  // Internal types (must be inside class for path-dependent types)
  //==========================================================================

  /**
   * Represents an interpreted object instance.
   */
  class InterpretedObject(
    val classSym: Symbol,
    val fields: mutable.Map[Symbol, LocalValue]
  ) {
    override def toString: String = s"InterpretedObject(${classSym.fullName})"

    def getField(sym: Symbol): Any = fields.get(sym) match {
      case Some(local) => local.get
      case None => throw new RuntimeException(s"Field ${sym.name} not found in ${classSym.fullName}")
    }
  }

  /**
   * Represents an interpreted closure (function) with captured environment.
   */
  class InterpretedClosure(
    val body: Term,
    val params: List[Symbol],
    val capturedEnv: Env
  ) {
    override def toString: String = s"InterpretedClosure(${params.map(_.name).mkString(", ")})"
  }

  /**
   * Represents a case class companion object.
   * Supports `apply` method to create case class instances.
   */
  class CaseClassCompanion(
    val caseClass: Symbol,
    val companionModule: Symbol
  ) {
    override def toString: String = s"CaseClassCompanion(${caseClass.fullName})"
  }

  /**
   * Extractor for closure definitions (must be inside class).
   */
  private object ClosureDef {
    def unapply(tree: Tree): Option[DefDef] = tree match {
      case Block(List(ddef: DefDef), Closure(_, _)) => Some(ddef)
      case Block(List(ddef: DefDef), Typed(Closure(_, _), _)) => Some(ddef)
      case _ => None
    }
  }

  /**
   * Marker objects for intrinsic modules (stdlib singletons with native implementations).
   */
  private object IntrinsicModule {
    case object Console
    case object Predef
    case object Math
    case object SomeFactory   // scala.Some companion object
    case object ListFactory   // scala.collection.immutable.List companion object
    case object NilModule     // scala.collection.immutable.Nil
    case object ConsFactory   // scala.collection.immutable.:: companion object
  }

  //==========================================================================
  // Object instantiation
  //==========================================================================

  def interpretNew(fn: Tree, argss: List[List[Term]]): Result = {
    val classSym = fn.symbol.owner
    val args = argss.flatten.map(arg => eval(arg))
    val className = classSym.fullName

    // DEBUG: Uncomment to trace class instantiation
    // if (className.contains("Tuple")) println(s"[DEBUG-NEW] className=$className args=$args")

    // Check for intrinsic classes first (JVM classes without TASTy)
    createIntrinsicInstance(className, args).getOrElse {
      // Get class definition from TASTy
      classSym.tree match {
        case classDef: ClassDef =>
          createInstance(classDef, fn.symbol, args)
        case _ =>
          throw new RuntimeException(s"Cannot create instance of $className: no TASTy available")
      }
    }
  }

  /**
   * Create instances of intrinsic classes (JVM classes without TASTy).
   */
  private def createIntrinsicInstance(className: String, args: List[AbstractAny]): Option[AbstractAny] = {
    className match {
      // Common exceptions
      case "java.lang.RuntimeException" =>
        Some(if (args.isEmpty) new RuntimeException() else new RuntimeException(args.head.toString))
      case "java.lang.Exception" =>
        Some(if (args.isEmpty) new Exception() else new Exception(args.head.toString))
      case "java.lang.IllegalArgumentException" =>
        Some(if (args.isEmpty) new IllegalArgumentException() else new IllegalArgumentException(args.head.toString))
      case "java.lang.IllegalStateException" =>
        Some(if (args.isEmpty) new IllegalStateException() else new IllegalStateException(args.head.toString))
      case "java.lang.NullPointerException" =>
        Some(if (args.isEmpty) new NullPointerException() else new NullPointerException(args.head.toString))
      case "java.lang.UnsupportedOperationException" =>
        Some(if (args.isEmpty) new UnsupportedOperationException() else new UnsupportedOperationException(args.head.toString))
      case "scala.MatchError" =>
        Some(new MatchError(args.headOption.orNull))

      // Common collections
      case "scala.Some" =>
        Some(scala.Some(args.head))
      case "scala.Tuple2" | "scala.Tuple2$" =>
        Some((args(0), args(1)))
      case "scala.Tuple3" | "scala.Tuple3$" =>
        Some((args(0), args(1), args(2)))
      case "scala.Tuple4" | "scala.Tuple4$" =>
        Some((args(0), args(1), args(2), args(3)))
      case "scala.Tuple5" | "scala.Tuple5$" =>
        Some((args(0), args(1), args(2), args(3), args(4)))

      // StringBuilder
      case "java.lang.StringBuilder" | "scala.collection.mutable.StringBuilder" =>
        Some(if (args.isEmpty) new StringBuilder() else new StringBuilder(args.head.toString))

      case _ => None
    }
  }

  /**
   * Create a new instance of a class by interpreting its constructor.
   */
  private def createInstance(classDef: ClassDef, ctorSym: Symbol, args: List[AbstractAny])(using Env): InterpretedObject = {
    val classSym = classDef.symbol

    // Create the object
    val obj = new InterpretedObject(classSym, mutable.Map.empty)

    // Find the constructor
    val ctor = classDef.body.collectFirst {
      case ddef: DefDef if ddef.symbol == ctorSym => ddef
    }.orElse {
      Some(classDef.constructor)
    }

    ctor match {
      case Some(ctorDef) =>
        // Bind constructor parameters
        val paramSymbols = ctorDef.termParamss.flatMap(_.params.map(_.symbol))
        val paramBindings = paramSymbols.zip(args.map(LocalValue.valFrom))

        // Create environment with `this` and parameters
        val ctorEnv: Env = summon[Env] ++ paramBindings + (classSym -> LocalValue.valFrom(obj))

        // Initialize fields from class body
        classDef.body.foreach {
          case vdef: ValDef if !vdef.symbol.flags.is(Flags.ParamAccessor) =>
            val value = vdef.rhs match {
              case Some(rhs) => eval(rhs)(using ctorEnv)
              case None => interpretUnit() // Uninitialized
            }
            obj.fields(vdef.symbol) = LocalValue.valFrom(value)

          case _ => // Skip methods and other definitions
        }

        // Copy constructor arguments to fields
        // For case classes, the constructor params are stored as fields (param accessors)
        paramSymbols.zip(args).foreach { case (sym, value) =>
          // Store all constructor parameters as fields
          obj.fields(sym) = LocalValue.valFrom(value)
        }

        // Also find and store param accessor vals from class body
        classDef.body.foreach {
          case vdef: ValDef if vdef.symbol.flags.is(Flags.ParamAccessor) =>
            // Find corresponding argument by name
            paramSymbols.zip(args).find(_._1.name == vdef.symbol.name).foreach { case (_, value) =>
              obj.fields(vdef.symbol) = LocalValue.valFrom(value)
            }
          case _ =>
        }

        obj

      case None =>
        throw new RuntimeException(s"No constructor found for ${classSym.fullName}")
    }
  }

  //==========================================================================
  // Method calls
  //==========================================================================

  override def interpretCall(fn: Term, argss: List[List[Term]]): Result = {
    // Check for intrinsics first (stdlib functions that need native implementation)
    val intrinsicResult = tryIntrinsic(fn, argss)
    if (intrinsicResult.isDefined) return intrinsicResult.get

    fn match {
      case Select(prefix, _) =>
        val receiver = eval(prefix)
        // Get the method def to check for by-name parameters
        val methodDef = tastyLoader.loadMethodDef(fn.symbol)
        val args = evaluateArgs(argss.flatten, methodDef)
        interpretMethodCallOnReceiver(receiver, fn.symbol, args)

      case _ =>
        // Static method call or local method
        val methodSym = fn.symbol
        tastyLoader.loadMethodDef(methodSym) match {
          case Some(ddef) =>
            val paramSymbols = ddef.termParamss.flatMap(_.params.map(_.symbol))
            val args = evaluateArgsForParams(argss.flatten, ddef)
            withLocalValues(paramSymbols, args) {
              eval(ddef.rhs.get)
            }
          case None =>
            // Try parent implementation
            super.interpretCall(fn, argss)
        }
    }
  }

  /**
   * Evaluate arguments, handling by-name parameters by wrapping them in thunks.
   */
  private def evaluateArgs(args: List[Term], methodDef: Option[DefDef])(using Env): List[AbstractAny] = {
    methodDef match {
      case Some(ddef) =>
        val params = ddef.termParamss.flatMap(_.params)
        args.zipWithIndex.map { case (arg, idx) =>
          val isByName = params.lift(idx).exists(p => isByNameType(p.tpt.tpe))
          if (isByName) {
            // Wrap in a thunk that captures current environment
            createByNameThunk(arg)
          } else {
            eval(arg)
          }
        }
      case None =>
        // No method def available, evaluate all eagerly
        args.map(arg => eval(arg))
    }
  }

  /**
   * Evaluate arguments for a method call, returning LocalValues for binding.
   */
  private def evaluateArgsForParams(args: List[Term], ddef: DefDef)(using Env): List[LocalValue] = {
    val params = ddef.termParamss.flatMap(_.params)
    args.zipWithIndex.map { case (arg, idx) =>
      val isByName = params.lift(idx).exists(p => isByNameType(p.tpt.tpe))
      if (isByName) {
        // Create a lazy LocalValue that evaluates on first access
        createByNameLocalValue(arg)
      } else {
        LocalValue.valFrom(eval(arg))
      }
    }
  }

  /**
   * Check if a type is a by-name type (=> T).
   */
  private def isByNameType(tpe: TypeRepr): Boolean = {
    tpe match {
      case ByNameType(_) => true
      case _ => false
    }
  }

  /**
   * Create a thunk for a by-name argument that evaluates on demand.
   */
  private def createByNameThunk(arg: Term)(using env: Env): AbstractAny = {
    // Return an InterpretedClosure with no parameters
    new InterpretedClosure(arg, Nil, env)
  }

  /**
   * Create a LocalValue for a by-name parameter that evaluates on each access.
   */
  private def createByNameLocalValue(arg: Term)(using env: Env): LocalValue = {
    new LocalValue {
      def get: AbstractAny = eval(arg)(using env)
    }
  }

  /**
   * Handle intrinsic functions - stdlib functions that need native implementation.
   * Returns Some(result) if this is an intrinsic, None otherwise.
   */
  private def tryIntrinsic(fn: Term, argss: List[List[Term]])(using Env): Option[AbstractAny] = {
    val methodSym = fn.symbol
    val ownerName = methodSym.owner.fullName
    val methodName = methodSym.name


    (ownerName, methodName) match {
      // Console/Predef print functions
      case ("scala.Console$" | "scala.Predef$", "println") if argss.flatten.isEmpty =>
        println()
        Some(())
      case ("scala.Console$" | "scala.Predef$", "println") =>
        val arg = eval(argss.flatten.head)
        println(arg)
        Some(())
      case ("scala.Console$" | "scala.Predef$", "print") =>
        val arg = eval(argss.flatten.head)
        print(arg)
        Some(())

      // String concatenation via StringContext
      case ("scala.StringContext", "s") =>
        // String interpolation - evaluate parts and args
        // s"Hello $name" desugars to StringContext.apply("Hello ", "").s(name)
        fn match {
          case Select(Apply(_, List(Typed(Repeated(parts, _), _))), _) =>
            val partStrings = parts.map(p => eval(p).toString)
            val argValues = argss.flatten.flatMap {
              case Typed(Repeated(elems, _), _) => elems.map(e => eval(e).toString)
              case arg => List(eval(arg).toString)
            }
            // Interleave parts and args
            val result = partStrings.zipAll(argValues, "", "").map { case (p, a) => p + a }.mkString
            Some(result)
          case Select(receiver, _) =>
            // Fallback: evaluate receiver to get StringContext, then build string
            val receiverVal = eval(receiver)
            receiverVal match {
              case sc: StringContext =>
                val argValues = argss.flatten.flatMap {
                  case Typed(Repeated(elems, _), _) => elems.map(e => eval(e))
                  case arg => List(eval(arg))
                }
                Some(sc.s(argValues*))
              case _ => None
            }
          case _ => None
        }

      // Common conversions
      case ("scala.Int$", "int2long") =>
        Some(eval(argss.flatten.head).asInstanceOf[Int].toLong)
      case ("scala.Int$", "int2double") =>
        Some(eval(argss.flatten.head).asInstanceOf[Int].toDouble)
      case ("scala.Int$", "int2float") =>
        Some(eval(argss.flatten.head).asInstanceOf[Int].toFloat)

      // Runtime exceptions - throw is a special operator
      case ("scala.runtime.Scala3RunTime$" | "<special-ops>", "throw") =>
        val exc = eval(argss.flatten.head)
        throw new InterpretedException(exc.asInstanceOf[Throwable])

      // List construction
      case ("scala.collection.immutable.List$" | "scala.package$", "apply") if methodName == "apply" =>
        // List.apply or List() - get the varargs
        val args = argss.flatten.flatMap {
          case Typed(Repeated(elems, _), _) => elems.map(eval(_))
          case arg => List(eval(arg))
        }
        Some(args.toList)

      case ("scala.collection.immutable.List$" | "scala.package$", "empty") =>
        Some(Nil)

      // :: constructor
      case ("scala.collection.immutable.$colon$colon$" | "scala.collection.immutable.::$", "apply") =>
        val flatArgs = argss.flatten
        val head = eval(flatArgs.head)
        val tail = eval(flatArgs(1)).asInstanceOf[List[Any]]
        Some(head :: tail)

      // Tuple constructors
      case ("scala.Tuple2$", "apply") =>
        val flatArgs = argss.flatten.map(eval(_))
        Some((flatArgs(0), flatArgs(1)))
      case ("scala.Tuple3$", "apply") =>
        val flatArgs = argss.flatten.map(eval(_))
        Some((flatArgs(0), flatArgs(1), flatArgs(2)))
      case ("scala.Tuple4$", "apply") =>
        val flatArgs = argss.flatten.map(eval(_))
        Some((flatArgs(0), flatArgs(1), flatArgs(2), flatArgs(3)))
      case ("scala.Tuple5$", "apply") =>
        val flatArgs = argss.flatten.map(eval(_))
        Some((flatArgs(0), flatArgs(1), flatArgs(2), flatArgs(3), flatArgs(4)))

      case _ => None
    }
  }

  /**
   * Call a method on a receiver object.
   */
  private def interpretMethodCallOnReceiver(receiver: AbstractAny, methodSym: Symbol, args: List[AbstractAny])(using Env): AbstractAny = {
    receiver match {
      // Intrinsic modules - stdlib singletons with native implementations
      case IntrinsicModule.Console | IntrinsicModule.Predef =>
        interpretConsoleMethod(methodSym.name, args)

      case IntrinsicModule.Math =>
        interpretMathMethod(methodSym.name, args)

      case IntrinsicModule.SomeFactory =>
        methodSym.name match {
          case "apply" => scala.Some(args.head)
          case "unapply" => args.head match {
            case s: scala.Some[?] => scala.Some(s.get)
            case _ => None
          }
          case _ => throw new RuntimeException(s"Unsupported Some method: ${methodSym.name}")
        }

      // List intrinsics
      case IntrinsicModule.ListFactory =>
        methodSym.name match {
          case "apply" =>
            // List.apply takes varargs, which comes as a Seq
            args.head match {
              case seq: Seq[?] => seq.toList
              case arr: Array[?] => arr.toList
              case _ => List(args.head)
            }
          case "empty" => Nil
          case _ => throw new RuntimeException(s"Unsupported List method: ${methodSym.name}")
        }

      case IntrinsicModule.NilModule =>
        methodSym.name match {
          case "unapply" => args.head match {
            case Nil => true
            case _ => false
          }
          case _ => throw new RuntimeException(s"Unsupported Nil method: ${methodSym.name}")
        }

      case IntrinsicModule.ConsFactory =>
        methodSym.name match {
          case "apply" =>
            // ::.apply(head, tail) creates a new list
            args.head :: args(1).asInstanceOf[List[Any]]
          case "unapply" => args.head match {
            case head :: tail => scala.Some((head, tail))
            case _ => None
          }
          case _ => throw new RuntimeException(s"Unsupported :: method: ${methodSym.name}")
        }

      case obj: InterpretedObject =>
        // Look up method in the object's class hierarchy
        val methodDef = findMethod(obj.classSym, methodSym)
        methodDef match {
          case Some(ddef) =>
            val paramSymbols = ddef.termParamss.flatMap(_.params.map(_.symbol))
            val argBindings = paramSymbols.zip(args.map(LocalValue.valFrom))

            // Create environment with `this` bound
            val methodEnv: Env = summon[Env] ++ argBindings + (obj.classSym -> LocalValue.valFrom(obj))
            eval(ddef.rhs.get)(using methodEnv)

          case None =>
            throw new RuntimeException(s"Method ${methodSym.name} not found on ${obj.classSym.fullName}")
        }

      case companion: CaseClassCompanion =>
        // Case class companion - support apply method
        methodSym.name match {
          case "apply" =>
            // Create a new instance of the case class
            companion.caseClass.tree match {
              case classDef: ClassDef =>
                createInstance(classDef, classDef.constructor.symbol, args)
              case _ =>
                throw new RuntimeException(s"Cannot find case class definition for ${companion.caseClass.fullName}")
            }
          case "unapply" =>
            // Extractor support
            val scrutinee = args.head
            scrutinee match {
              case obj: InterpretedObject =>
                // Check if the object is an instance of this case class
                // Compare by full name since symbols might differ between contexts
                val isInstance = obj.classSym.fullName == companion.caseClass.fullName ||
                                 obj.classSym == companion.caseClass
                if (isInstance) {
                  // Get all field values in order
                  val classDef = companion.caseClass.tree.asInstanceOf[ClassDef]
                  val params = classDef.constructor.termParamss.flatMap(_.params.map(_.symbol))
                  val fieldValues = params.flatMap(p => obj.fields.get(p).map(_.get))
                  if (fieldValues.isEmpty) {
                    // Try getting all fields in insertion order
                    val allValues = obj.fields.values.map(_.get).toList
                    if (allValues.size == 1) {
                      scala.Some(allValues.head)
                    } else {
                      scala.Some(Tuple.fromArray(allValues.toArray))
                    }
                  } else if (fieldValues.size == 1) {
                    scala.Some(fieldValues.head)
                  } else {
                    scala.Some(Tuple.fromArray(fieldValues.toArray))
                  }
                } else {
                  None
                }
              case _ => None
            }
          case _ =>
            throw new RuntimeException(s"Unsupported case class companion method: ${methodSym.name}")
        }

      case closure: InterpretedClosure =>
        // Closure application - apply method
        methodSym.name match {
          case "apply" => applyClosure(closure, args)
          case _ => throw new RuntimeException(s"Unsupported method on closure: ${methodSym.name}")
        }

      case _ =>
        // Primitive or external object - delegate to host
        interpretPrimitiveMethodCall(receiver, methodSym, args)
    }
  }

  /**
   * Handle Console/Predef methods.
   */
  private def interpretConsoleMethod(methodName: String, args: List[AbstractAny]): AbstractAny = {
    methodName match {
      case "println" if args.isEmpty => println(); ()
      case "println" => println(args.head); ()
      case "print" => print(args.head); ()
      case "readLine" if args.isEmpty => scala.io.StdIn.readLine()
      case "readLine" => scala.io.StdIn.readLine(args.head.toString)
      case _ => throw new RuntimeException(s"Unsupported Console method: $methodName")
    }
  }

  /**
   * Handle Math methods.
   */
  private def interpretMathMethod(methodName: String, args: List[AbstractAny]): AbstractAny = {
    methodName match {
      case "abs" => args.head match {
        case i: Int => math.abs(i)
        case l: Long => math.abs(l)
        case f: Float => math.abs(f)
        case d: Double => math.abs(d)
      }
      case "max" => (args(0), args(1)) match {
        case (a: Int, b: Int) => math.max(a, b)
        case (a: Long, b: Long) => math.max(a, b)
        case (a: Double, b: Double) => math.max(a, b)
      }
      case "min" => (args(0), args(1)) match {
        case (a: Int, b: Int) => math.min(a, b)
        case (a: Long, b: Long) => math.min(a, b)
        case (a: Double, b: Double) => math.min(a, b)
      }
      case "sqrt" => math.sqrt(args.head.asInstanceOf[Double])
      case "pow" => math.pow(args(0).asInstanceOf[Double], args(1).asInstanceOf[Double])
      case _ => throw new RuntimeException(s"Unsupported Math method: $methodName")
    }
  }

  /**
   * Find a method in the class hierarchy.
   */
  private def findMethod(classSym: Symbol, methodSym: Symbol): Option[DefDef] = {
    // First try direct lookup
    tastyLoader.loadMethodDef(methodSym).orElse {
      // Try to find by name in class members
      classSym.memberMethods.find(_.name == methodSym.name).flatMap { m =>
        tastyLoader.loadMethodDef(m)
      }
    }
  }

  /**
   * Handle method calls on primitives and host platform objects.
   */
  private def interpretPrimitiveMethodCall(receiver: AbstractAny, methodSym: Symbol, args: List[AbstractAny])(using Env): AbstractAny = {
    val methodName = methodSym.name

    // String methods
    receiver match {
      case s: String =>
        methodName match {
          case "length" => s.length
          case "charAt" => s.charAt(args.head.asInstanceOf[Int])
          case "substring" if args.size == 1 => s.substring(args.head.asInstanceOf[Int])
          case "substring" if args.size == 2 => s.substring(args(0).asInstanceOf[Int], args(1).asInstanceOf[Int])
          case "+" | "$plus" => s + args.head.toString
          case "startsWith" => s.startsWith(args.head.asInstanceOf[String])
          case "endsWith" => s.endsWith(args.head.asInstanceOf[String])
          case "contains" => s.contains(args.head.asInstanceOf[CharSequence])
          case "isEmpty" => s.isEmpty
          case "nonEmpty" => s.nonEmpty
          case "toString" => s.toString
          case "hashCode" => s.hashCode
          case "equals" => s.equals(args.head)
          case _ => throw new RuntimeException(s"Unsupported String method: $methodName")
        }

      // Collections - delegate to actual collection methods
      case list: List[?] =>
        methodName match {
          case "head" => list.head
          case "tail" => list.tail
          case "isEmpty" => list.isEmpty
          case "nonEmpty" => list.nonEmpty
          case "size" | "length" => list.size
          case "apply" => list.apply(args.head.asInstanceOf[Int])
          case "toString" => list.toString
          case "hashCode" => list.hashCode
          case "equals" => list.equals(args.head)
          case "map" => list.map(makeFn1(args.head))
          case "flatMap" => list.flatMap(x => makeFn1(args.head)(x).asInstanceOf[IterableOnce[Any]])
          case "filter" => list.filter(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "withFilter" => list.withFilter(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "foreach" => list.foreach(makeFn1(args.head)); ()
          case "foldLeft" => list.foldLeft(args(0))(makeFn2(args(1)))
          case "mkString" if args.isEmpty => list.mkString
          case "mkString" if args.size == 1 => list.mkString(args.head.asInstanceOf[String])
          case "mkString" if args.size == 3 => list.mkString(args(0).asInstanceOf[String], args(1).asInstanceOf[String], args(2).asInstanceOf[String])
          case "::" | "$colon$colon" => args.head :: list
          case "++" | "$plus$plus" => list ++ args.head.asInstanceOf[Iterable[?]]
          case _ => throw new RuntimeException(s"Unsupported List method: $methodName")
        }

      case opt: Option[?] =>
        methodName match {
          case "isEmpty" => opt.isEmpty
          case "nonEmpty" => opt.nonEmpty
          case "isDefined" => opt.isDefined
          case "get" => opt.get
          case "getOrElse" => opt.getOrElse(evalThunk(args.head))
          case "map" => opt.map(makeFn1(args.head))
          case "flatMap" => opt.flatMap(makeFn1(args.head).andThen(_.asInstanceOf[Option[?]]))
          case "filter" => opt.filter(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "fold" => opt.fold(evalThunk(args(0)))(makeFn1(args(1)))
          case "orElse" => opt.orElse(evalThunk(args.head).asInstanceOf[Option[?]])
          case "toString" => opt.toString
          case "hashCode" => opt.hashCode
          case "equals" => opt.equals(args.head)
          case _ => throw new RuntimeException(s"Unsupported Option method: $methodName")
        }

      case seq: Seq[?] =>
        methodName match {
          case "head" => seq.head
          case "tail" => seq.tail
          case "isEmpty" => seq.isEmpty
          case "nonEmpty" => seq.nonEmpty
          case "size" | "length" => seq.size
          case "apply" => seq.apply(args.head.asInstanceOf[Int])
          case "map" => seq.map(makeFn1(args.head))
          case "flatMap" => seq.flatMap(makeFn1(args.head).andThen(_.asInstanceOf[IterableOnce[?]]))
          case "filter" => seq.filter(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "withFilter" => seq.withFilter(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "foreach" => seq.foreach(makeFn1(args.head)); ()
          case "toList" => seq.toList
          case "toSeq" => seq.toSeq
          case "toString" => seq.toString
          case _ => throw new RuntimeException(s"Unsupported Seq method: $methodName")
        }

      // WithFilter - returned by withFilter for for-comprehensions with guards
      case wf: scala.collection.WithFilter[?, ?] =>
        methodName match {
          case "map" => wf.map(makeFn1(args.head))
          case "flatMap" => wf.flatMap(makeFn1(args.head).andThen(_.asInstanceOf[IterableOnce[?]]))
          case "foreach" => wf.foreach(makeFn1(args.head)); ()
          case "withFilter" => wf.withFilter(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case _ => throw new RuntimeException(s"Unsupported WithFilter method: $methodName")
        }

      // Handle exceptions
      case e: Throwable =>
        methodName match {
          case "getMessage" => e.getMessage
          case "getCause" => e.getCause
          case "toString" => e.toString
          case "hashCode" => e.hashCode
          case "equals" => e.equals(args.head)
          case "getStackTrace" => e.getStackTrace
          case "printStackTrace" => e.printStackTrace(); ()
          case _ => throw new RuntimeException(s"Unsupported Throwable method: $methodName")
        }

      // Handle List operations
      case list: List[?] =>
        methodName match {
          case "isEmpty" => list.isEmpty
          case "nonEmpty" => list.nonEmpty
          case "head" => list.head
          case "tail" => list.tail
          case "length" | "size" => list.length
          case "reverse" => list.reverse
          case "::" | "$colon$colon" => args.head :: list
          case "+:" | "$plus$colon" => args.head +: list
          case ":+" | "$colon$plus" => list :+ args.head
          case "++" | "$plus$plus" => list ++ args.head.asInstanceOf[Iterable[?]]
          case "take" => list.take(args.head.asInstanceOf[Int])
          case "drop" => list.drop(args.head.asInstanceOf[Int])
          case "contains" => list.contains(args.head)
          case "indexOf" => list.indexOf(args.head)
          case "mkString" =>
            if (args.isEmpty) list.mkString
            else if (args.length == 1) list.mkString(args.head.toString)
            else list.mkString(args(0).toString, args(1).toString, args(2).toString)
          case "toString" => list.toString
          case "hashCode" => list.hashCode
          case "equals" => list.equals(args.head)
          case "map" => list.map(makeFn1(args.head))
          case "flatMap" => list.flatMap(makeFn1(args.head).andThen(_.asInstanceOf[IterableOnce[?]]))
          case "filter" => list.filter(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "foreach" => list.foreach(makeFn1(args.head)); ()
          case "foldLeft" => list.foldLeft(args(0))(makeFn2(args(1)))
          case "foldRight" => list.foldRight(args(0))(makeFn2(args(1)))
          case "reduce" => list.reduce(makeFn2(args.head))
          case "exists" => list.exists(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "forall" => list.forall(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "find" => list.find(makeFn1(args.head).andThen(_.asInstanceOf[Boolean]))
          case "zip" => list.zip(args.head.asInstanceOf[Iterable[?]])
          case "zipWithIndex" => list.zipWithIndex
          case "getClass" => list.getClass
          case _ => throw new RuntimeException(s"Unsupported List method: $methodName")
        }

      // Handle boxed primitives and Any type
      case i: java.lang.Integer =>
        methodName match {
          case "toString" => i.toString
          case "hashCode" => i.hashCode
          case "equals" => i.equals(args.head)
          case "intValue" => i.intValue
          case _ => throw new RuntimeException(s"Unsupported Integer method: $methodName")
        }

      case l: java.lang.Long =>
        methodName match {
          case "toString" => l.toString
          case "hashCode" => l.hashCode
          case "equals" => l.equals(args.head)
          case "longValue" => l.longValue
          case _ => throw new RuntimeException(s"Unsupported Long method: $methodName")
        }

      case d: java.lang.Double =>
        methodName match {
          case "toString" => d.toString
          case "hashCode" => d.hashCode
          case "equals" => d.equals(args.head)
          case "doubleValue" => d.doubleValue
          case _ => throw new RuntimeException(s"Unsupported Double method: $methodName")
        }

      case b: java.lang.Boolean =>
        methodName match {
          case "toString" => b.toString
          case "hashCode" => b.hashCode
          case "equals" => b.equals(args.head)
          case "booleanValue" => b.booleanValue
          case _ => throw new RuntimeException(s"Unsupported Boolean method: $methodName")
        }

      // Any object with toString/hashCode/equals
      case other =>
        methodName match {
          case "toString" => other.toString
          case "hashCode" => other.hashCode
          case "equals" => other.equals(args.head)
          case "getClass" => other.getClass
          case _ =>
            // Try JVM reflection for module methods as a fallback
            tryJvmReflectionCall(other, methodName, args).getOrElse {
              throw new RuntimeException(s"Cannot call method $methodName on ${receiver.getClass}: not a supported type")
            }
        }
    }
  }

  /**
   * Try calling a method on a JVM object using reflection.
   * This is a fallback for stdlib modules that are loaded from JVM.
   */
  private def tryJvmReflectionCall(receiver: Any, methodName: String, args: List[AbstractAny]): Option[AbstractAny] = {
    try {
      val clazz = receiver.getClass
      // Find a method with matching name and arg count
      val methods = clazz.getMethods.filter(m => m.getName == methodName && m.getParameterCount == args.size)
      methods.headOption match {
        case Some(method) =>
          val result = method.invoke(receiver, args.map(_.asInstanceOf[AnyRef])*)
          Some(result)
        case None =>
          // Try with varargs (common for List.apply, etc.)
          val varargMethods = clazz.getMethods.filter { m =>
            m.getName == methodName && m.isVarArgs && m.getParameterCount == 1
          }
          varargMethods.headOption match {
            case Some(method) =>
              // Wrap args in a Seq for varargs
              val seqArg = args match {
                case List(seq: Seq[?]) => seq
                case other => other
              }
              val result = method.invoke(receiver, seqArg)
              Some(result)
            case None => None
          }
      }
    } catch {
      case e: Exception =>
        None
    }
  }

  /**
   * Convert an interpreted closure or function to a Scala function.
   */
  private def makeFn1(f: AbstractAny)(using Env): Any => Any = f match {
    case closure: InterpretedClosure =>
      (x: Any) => applyClosure(closure, List(x))
    case fn: (Any => Any) @unchecked =>
      fn
    case _ =>
      throw new RuntimeException(s"Expected function, got ${f.getClass}")
  }

  private def makeFn2(f: AbstractAny)(using Env): (Any, Any) => Any = f match {
    case closure: InterpretedClosure =>
      (x: Any, y: Any) => applyClosure(closure, List(x, y))
    case fn: ((Any, Any) => Any) @unchecked =>
      fn
    case _ =>
      throw new RuntimeException(s"Expected function, got ${f.getClass}")
  }

  private def evalThunk(thunk: AbstractAny)(using Env): AbstractAny = thunk match {
    case closure: InterpretedClosure if closure.params.isEmpty =>
      applyClosure(closure, Nil)
    case fn: (() => Any) @unchecked =>
      fn()
    case value =>
      value
  }

  /**
   * Apply a closure with arguments.
   */
  private def applyClosure(closure: InterpretedClosure, args: List[AbstractAny]): AbstractAny = {
    val env: Env = closure.capturedEnv ++
      closure.params.zip(args.map(LocalValue.valFrom))
    eval(closure.body)(using env)
  }

  //==========================================================================
  // Override interpretValGet to handle module references
  //==========================================================================

  override def interpretValGet(fn: Term): Result = {
    val sym = fn.symbol
    // Check if this is a module (object) reference
    if (sym.flags.is(Flags.Module)) {
      getModuleValue(sym)
    } else {
      // Check if it's in the environment, otherwise try to get module
      summon[Env].get(sym) match {
        case Some(local) => local.get
        case None =>
          // Maybe it's a top-level val or a module we haven't loaded
          if (sym.owner.flags.is(Flags.Module)) {
            // It's a val inside a module - get the module and then the val
            val moduleValue = getModuleValue(sym.owner)
            moduleValue match {
              case obj: InterpretedObject => obj.getField(sym)
              case jvmObj =>
                // Try to get the field from a JVM object using reflection
                val fieldName = sym.name
                // Convert Scala field names to JVM names
                val jvmFieldName = fieldName match {
                  case "Nil" => "Nil"  // scala.package$.Nil
                  case name => name
                }
                try {
                  val field = jvmObj.getClass.getMethod(jvmFieldName)
                  field.invoke(jvmObj)
                } catch {
                  case _: NoSuchMethodException =>
                    // Try as a field
                    try {
                      val field = jvmObj.getClass.getField(jvmFieldName)
                      field.get(jvmObj)
                    } catch {
                      case _: NoSuchFieldException =>
                        throw new RuntimeException(s"Cannot get val ${sym.name} from ${sym.owner.fullName}")
                    }
                }
            }
          } else {
            throw new NoSuchElementException(s"key not found: ${sym.name} (${sym.fullName})")
          }
      }
    }
  }

  //==========================================================================
  // Override eval to handle additional tree types
  //==========================================================================

  override def eval(tree: Statement): Result = {
    tree match {
      // Match expressions
      case Match(selector, cases) =>
        log("interpretMatch", tree)(interpretMatch(selector, cases))

      // Try/Catch/Finally
      case Try(block, catches, finalizer) =>
        log("interpretTry", tree)(interpretTry(block, catches, finalizer))

      // Return
      case Return(expr, from) =>
        log("interpretReturn", tree)(interpretReturn(expr))

      // Closure (lambda)
      case Closure(meth, tpt) =>
        log("interpretClosure", tree)(interpretClosure(meth, tpt))

      // This reference
      case This(qual) =>
        log("interpretThis", tree)(interpretThis(qual))

      // Named argument (unwrap)
      case NamedArg(_, arg) =>
        eval(arg)

      // Inlined code
      case Inlined(call, bindings, expansion) =>
        log("interpretInlined", tree)(interpretBlock(bindings, expansion))

      // Closure definition
      case ClosureDef(ddef) =>
        log("interpretClosureDef", tree)(interpretClosureDef(ddef))

      // Class definition - skip in block context (returns Unit)
      case ClassDef(name, _, _, _, _) =>
        log("skipClassDef", tree)(())

      // Default to parent implementation
      case _ =>
        super.eval(tree)
    }
  }

  //==========================================================================
  // Match expression
  //==========================================================================

  private def interpretMatch(selector: Term, cases: List[CaseDef])(using Env): AbstractAny = {
    val scrutinee = eval(selector)

    cases.find(caseDef => matchPattern(scrutinee, caseDef.pattern)) match {
      case Some(caseDef) =>
        val bindings = extractBindings(scrutinee, caseDef.pattern)
        val guardPasses = caseDef.guard match {
          case Some(guard) => eval(guard)(using summon[Env] ++ bindings).asInstanceOf[Boolean]
          case None => true
        }

        if (guardPasses) {
          eval(caseDef.rhs)(using summon[Env] ++ bindings)
        } else {
          // Try remaining cases
          val remainingCases = cases.dropWhile(_ != caseDef).tail
          if (remainingCases.nonEmpty) {
            interpretMatchCases(scrutinee, remainingCases)
          } else {
            throw new MatchError(s"No case matched (guard failed): $scrutinee")
          }
        }

      case None =>
        throw new MatchError(s"No case matched: $scrutinee")
    }
  }

  private def interpretMatchCases(scrutinee: AbstractAny, cases: List[CaseDef])(using Env): AbstractAny = {
    cases.find(caseDef => matchPattern(scrutinee, caseDef.pattern)) match {
      case Some(caseDef) =>
        val bindings = extractBindings(scrutinee, caseDef.pattern)
        val guardPasses = caseDef.guard match {
          case Some(guard) => eval(guard)(using summon[Env] ++ bindings).asInstanceOf[Boolean]
          case None => true
        }

        if (guardPasses) {
          eval(caseDef.rhs)(using summon[Env] ++ bindings)
        } else {
          val remainingCases = cases.dropWhile(_ != caseDef).tail
          if (remainingCases.nonEmpty) {
            interpretMatchCases(scrutinee, remainingCases)
          } else {
            throw new MatchError(s"No case matched (guard failed): $scrutinee")
          }
        }

      case None =>
        throw new MatchError(s"No case matched: $scrutinee")
    }
  }

  /**
   * Check if a pattern matches a scrutinee.
   */
  private def matchPattern(scrutinee: AbstractAny, pattern: Tree)(using Env): Boolean = pattern match {
    case Wildcard() => true

    case Bind(_, inner) => matchPattern(scrutinee, inner)

    case Literal(const) => scrutinee == const.value

    case Typed(Wildcard(), tpt) =>
      // Type pattern: case _: Int =>
      isInstanceOfType(scrutinee, tpt)

    case Typed(inner, tpt) =>
      // Typed pattern wrapping another pattern (e.g., Some(x): Option[Int])
      // First check the type, then match the inner pattern
      isInstanceOfType(scrutinee, tpt) && matchPattern(scrutinee, inner)

    case TypedOrTest(inner, tpt) =>
      // TypedOrTest is used for patterns like `case Some(x) =>` where
      // the pattern is typed but also needs to be tested
      isInstanceOfType(scrutinee, tpt) && matchPattern(scrutinee, inner)

    case Alternatives(patterns) => patterns.exists(p => matchPattern(scrutinee, p))

    case Unapply(fun, implicits, patterns) =>
      // Extractor pattern
      val extractorResult = interpretExtractor(scrutinee, fun, implicits)
      // Debug disabled: println(s"[DEBUG-UNAPPLY-MATCH] scrutinee=$scrutinee result=$extractorResult")
      extractorResult match {
        case Some(extracted) =>
          extracted match {
            case tuple: Product if patterns.size > 1 =>
              patterns.zipWithIndex.forall { case (pat, i) =>
                matchPattern(tuple.productElement(i), pat)
              }
            case single if patterns.size == 1 =>
              matchPattern(single, patterns.head)
            case () if patterns.isEmpty =>
              true
            case _ => false
          }
        case None => false
        case true => patterns.isEmpty  // Boolean extractor
        case false => false
      }

    case ref: Ident if ref.symbol.flags.is(Flags.Module) =>
      // Object pattern (e.g., case None =>)
      val moduleValue = getModuleValue(ref.symbol)
      // Special handling for Nil (which might return IntrinsicModule.NilModule)
      val actualValue = moduleValue match {
        case IntrinsicModule.NilModule => Nil
        case other => other
      }
      scrutinee == actualValue

    case ref: Ident =>
      // Variable pattern - always matches
      // BUT: check if this is a known module first (Nil might not have Module flag set correctly)
      val name = ref.name
      if (name == "Nil") {
        // Special case: Nil should match empty list
        scrutinee match {
          case Nil => true
          case _ => false
        }
      } else if (name == "None") {
        // Special case: None singleton
        scrutinee match {
          case None => true
          case _ => false
        }
      } else {
        // Variable pattern - always matches
        true
      }

    case _ =>
      // Debug: show what kind of tree this is
      val treeType = pattern.getClass.getSimpleName
      throw new RuntimeException(s"Unsupported pattern ($treeType): ${pattern.show}\n${pattern.show(using Printer.TreeStructure)}")
  }

  /**
   * Extract bindings from a pattern match.
   */
  private def extractBindings(scrutinee: AbstractAny, pattern: Tree)(using Env): Map[Symbol, LocalValue] = pattern match {
    case Wildcard() => Map.empty

    case Bind(_, inner) =>
      val innerBindings = extractBindings(scrutinee, inner)
      innerBindings + (pattern.symbol -> LocalValue.valFrom(scrutinee))

    case Typed(Wildcard(), _) =>
      // Type pattern - no bindings
      Map.empty

    case Typed(inner, _) =>
      // Typed pattern wrapping another pattern
      extractBindings(scrutinee, inner)

    case TypedOrTest(inner, _) =>
      // TypedOrTest pattern
      extractBindings(scrutinee, inner)

    case Unapply(fun, implicits, patterns) =>
      val extractorResult = interpretExtractor(scrutinee, fun, implicits)
      // Debug disabled
      // println(s"[DEBUG-UNAPPLY-BIND] extracted=$extractorResult patterns=${patterns.map(_.show)}")
      extractorResult match {
        case Some(extracted) =>
          extracted match {
            case tuple: Product if patterns.size > 1 =>
              patterns.zipWithIndex.flatMap { case (pat, i) =>
                val element = tuple.productElement(i)
                // println(s"[DEBUG-UNAPPLY-BIND] binding pattern ${pat.show} to element $element")
                extractBindings(element, pat)
              }.toMap
            case single if patterns.size == 1 =>
              extractBindings(single, patterns.head)
            case _ => Map.empty
          }
        case _ => Map.empty
      }

    case Alternatives(patterns) =>
      // Find the matching alternative and extract its bindings
      patterns.find(p => matchPattern(scrutinee, p)) match {
        case Some(p) => extractBindings(scrutinee, p)
        case None => Map.empty
      }

    case _ => Map.empty
  }

  /**
   * Interpret an extractor pattern (unapply).
   */
  private def interpretExtractor(scrutinee: AbstractAny, fun: Term, implicits: List[Term])(using Env): Any = {
    // Get the unapply method
    val unapplyMethod = fun.symbol
    val ownerSym = unapplyMethod.owner
    val ownerName = ownerSym.fullName

    // Handle common stdlib extractors directly
    ownerName match {
      case "scala.Some" | "scala.Some$" =>
        // Some.unapply[A](x: Some[A]): Some[A] = x
        scrutinee match {
          case s: scala.Some[?] => Some(s.get)
          case _ => None
        }

      case "scala.None" | "scala.None$" =>
        // None doesn't have unapply - it's an object pattern
        if (scrutinee == None) Some(()) else None

      case "scala.Option" | "scala.Option$" =>
        // Option.unapply is like Some.unapply
        scrutinee match {
          case Some(x) => Some(x)
          case None => None
          case _ => None
        }

      case "scala.::" | "scala.$colon$colon" | "scala.collection.immutable.::" | "scala.collection.immutable.$colon$colon" =>
        // List cons extractor
        scrutinee match {
          case head :: tail => Some((head, tail))
          case _ => None
        }

      case "scala.Tuple2" | "scala.Tuple2$" =>
        scrutinee match {
          case (a, b) => Some((a, b))
          case _ => None
        }

      case _ =>
        // Handle case class extractors for InterpretedObject
        scrutinee match {
          case obj: InterpretedObject =>
            // Check if this is a case class extractor matching the object's class
            // The owner of unapply is the companion object, whose companion class should match
            val companionClass = ownerSym.companionClass
            if (companionClass.exists && (obj.classSym == companionClass || obj.classSym.fullName == companionClass.fullName)) {
              // Extract the constructor parameters
              // Case class fields are stored with param accessor symbols
              val classFields = obj.classSym.tree match {
                case classDef: ClassDef =>
                  // Get primary constructor parameters
                  classDef.constructor.termParamss.flatMap(_.params.map { param =>
                    obj.fields.get(param.symbol).map(_.get).getOrElse {
                      // Try to find by name
                      obj.fields.find(_._1.name == param.name).map(_._2.get).getOrElse {
                        throw new RuntimeException(s"Field ${param.name} not found in ${obj.classSym.fullName}")
                      }
                    }
                  })
                case _ => Nil
              }

              // Return as tuple or single value wrapped in Some
              classFields match {
                case Nil => Some(())
                case List(single) => Some(single)
                case multiple => Some(Tuple.fromArray(multiple.toArray))
              }
            } else {
              // Not a match
              None
            }

          case _ =>
            // Get the module instance for other extractors
            val moduleValue = if (ownerSym.flags.is(Flags.Module)) {
              getModuleValue(ownerSym)
            } else {
              fun match {
                case Select(prefix, _) => eval(prefix)
                case _ => throw new RuntimeException(s"Cannot get unapply receiver from $fun")
              }
            }

            // Call the unapply method
            interpretMethodCallOnReceiver(moduleValue, unapplyMethod, scrutinee :: implicits.map(eval(_)))
        }
    }
  }

  /**
   * Get a module (object) value.
   */
  private def getModuleValue(moduleSym: Symbol)(using Env): AbstractAny = {
    // Check if it's a known singleton / intrinsic module
    val fullName = moduleSym.fullName
    getIntrinsicModule(fullName).getOrElse {
      // Try to look up in environment
      summon[Env].get(moduleSym) match {
        case Some(local) => local.get
        case None =>
          // Create a new instance
          moduleSym.tree match {
            case classDef: ClassDef =>
              val obj = new InterpretedObject(moduleSym, mutable.Map.empty)
              // Initialize the module
              classDef.body.foreach {
                case vdef: ValDef if !vdef.symbol.flags.is(Flags.ParamAccessor) =>
                  val value = vdef.rhs match {
                    case Some(rhs) => eval(rhs)(using summon[Env] + (moduleSym -> LocalValue.valFrom(obj)))
                    case None => interpretUnit()
                  }
                  obj.fields(vdef.symbol) = LocalValue.valFrom(value)
                case _ =>
              }
              obj
            case other =>
              // For case class companions, the tree might be different
              // Try to check if this is a companion object for a case class
              val companionClass = moduleSym.companionClass
              if (companionClass.exists && companionClass.flags.is(Flags.Case)) {
                // It's a case class companion - create a special wrapper that supports `apply`
                new CaseClassCompanion(companionClass, moduleSym)
              } else {
                throw new RuntimeException(s"Cannot instantiate module ${moduleSym.fullName} (tree: ${if (other == null) "null" else other.getClass})")
              }
          }
      }
    }
  }

  /**
   * Get an intrinsic module - stdlib singletons that we provide native implementations for.
   */
  private def getIntrinsicModule(fullName: String): Option[AbstractAny] = fullName match {
    // Standard singletons
    case "scala.None" | "scala.None$" => Some(None)
    case "scala.collection.immutable.Nil" | "scala.Nil" | "scala.collection.immutable.Nil$" => Some(Nil)

    // Option factory
    case "scala.Some" | "scala.Some$" => Some(IntrinsicModule.SomeFactory)

    // List factories
    case "scala.collection.immutable.List" | "scala.collection.immutable.List$" | "scala.List" | "scala.List$" =>
      Some(IntrinsicModule.ListFactory)
    case "scala.collection.immutable.::" | "scala.collection.immutable.::$" | "scala.::" | "scala.::$" =>
      Some(IntrinsicModule.ConsFactory)
    // Nil for unapply pattern
    case "scala.package$.Nil" =>
      Some(IntrinsicModule.NilModule)

    // Collection library modules - return JVM instances directly
    case name if name.startsWith("scala.collection") || name.startsWith("scala.package$") =>
      // Try to load the module from the JVM runtime
      try {
        // Convert Scala names to JVM names
        val jvmName = name.stripSuffix("$")
          .replace("+:", "$plus$colon")
          .replace("::", "$colon$colon")
          .replace(":+", "$colon$plus")
          .replace("++", "$plus$plus")
        val clazz = Class.forName(jvmName + "$")
        val moduleField = clazz.getField("MODULE$")
        Some(moduleField.get(null))
      } catch {
        case _: Exception => None
      }

    // Console/Predef - return a marker object that intrinsic handlers recognize
    case "scala.Console" | "scala.Console$" => Some(IntrinsicModule.Console)
    case "scala.Predef" | "scala.Predef$" => Some(IntrinsicModule.Predef)

    // Math
    case "scala.math.package" | "scala.math.package$" => Some(IntrinsicModule.Math)

    case _ => None
  }

  private def isInstanceOfType(value: AbstractAny, tpt: TypeTree): Boolean = {
    val tpe = tpt.tpe
    val typeSymbol = tpe.typeSymbol

    // First, check for exact primitive type matches
    // This is important because all primitives are AnyVal, so we need to match specifically
    val typeName = typeSymbol.fullName

    value match {
      // Primitives - check exact type first
      case _: Int =>
        typeName == "scala.Int" || typeName == "scala.AnyVal" || typeName == "scala.Any"
      case _: Long =>
        typeName == "scala.Long" || typeName == "scala.AnyVal" || typeName == "scala.Any"
      case _: Double =>
        typeName == "scala.Double" || typeName == "scala.AnyVal" || typeName == "scala.Any"
      case _: Float =>
        typeName == "scala.Float" || typeName == "scala.AnyVal" || typeName == "scala.Any"
      case _: Boolean =>
        typeName == "scala.Boolean" || typeName == "scala.AnyVal" || typeName == "scala.Any"
      case _: Char =>
        typeName == "scala.Char" || typeName == "scala.AnyVal" || typeName == "scala.Any"
      case _: Byte =>
        typeName == "scala.Byte" || typeName == "scala.AnyVal" || typeName == "scala.Any"
      case _: Short =>
        typeName == "scala.Short" || typeName == "scala.AnyVal" || typeName == "scala.Any"

      // Reference types
      case _: String =>
        typeName == "java.lang.String" || typeName == "scala.Predef.String" ||
        tpe <:< TypeRepr.of[String] || typeName == "scala.Any" || typeName == "scala.AnyRef"
      case _: List[?] =>
        tpe <:< TypeRepr.of[List[?]] || tpe <:< TypeRepr.of[Seq[?]] || typeName == "scala.Any"
      case _: scala.Some[?] =>
        tpe <:< TypeRepr.of[Option[?]] || tpe <:< TypeRepr.of[Some[?]] || typeName == "scala.Any"
      case None =>
        tpe <:< TypeRepr.of[Option[?]] || typeName == "scala.None" || typeName == "scala.Any"
      case _: Option[?] =>
        tpe <:< TypeRepr.of[Option[?]] || typeName == "scala.Any"

      // Interpreted objects
      case obj: InterpretedObject =>
        obj.classSym.typeRef <:< tpe || typeName == "scala.Any"

      // null matches any reference type
      case null =>
        !(tpe <:< TypeRepr.of[AnyVal])

      // Handle exceptions - important for catch clauses
      case e: Throwable =>
        // Normalize type name - handle scala package aliases
        val normalizedTypeName = typeName match {
          case "scala.package$.RuntimeException" | "scala.RuntimeException" => "java.lang.RuntimeException"
          case "scala.package$.Exception" | "scala.Exception" => "java.lang.Exception"
          case "scala.package$.Throwable" | "scala.Throwable" => "java.lang.Throwable"
          case "scala.package$.IllegalArgumentException" | "scala.IllegalArgumentException" => "java.lang.IllegalArgumentException"
          case "scala.package$.NullPointerException" | "scala.NullPointerException" => "java.lang.NullPointerException"
          case "scala.package$.UnsupportedOperationException" | "scala.UnsupportedOperationException" => "java.lang.UnsupportedOperationException"
          case other => other
        }

        normalizedTypeName match {
          case "java.lang.Throwable" => true
          case "java.lang.Exception" => e.isInstanceOf[Exception]
          case "java.lang.RuntimeException" => e.isInstanceOf[RuntimeException]
          case "java.lang.IllegalArgumentException" => e.isInstanceOf[IllegalArgumentException]
          case "java.lang.IllegalStateException" => e.isInstanceOf[IllegalStateException]
          case "java.lang.NullPointerException" => e.isInstanceOf[NullPointerException]
          case "java.lang.UnsupportedOperationException" => e.isInstanceOf[UnsupportedOperationException]
          case "java.lang.Error" => e.isInstanceOf[Error]
          case "scala.MatchError" => e.isInstanceOf[MatchError]
          case "scala.Any" | "scala.AnyRef" | "java.lang.Object" => true
          case _ =>
            // Try class hierarchy check
            try {
              val targetClass = Class.forName(normalizedTypeName)
              targetClass.isInstance(e)
            } catch {
              case _: ClassNotFoundException => false
            }
        }

      // For other JVM objects, try Class.isInstance
      case other =>
        try {
          // Try to get the runtime class and check
          val runtimeClass = other.getClass
          val exactMatch = typeName == runtimeClass.getName
          val anyMatch = typeName == "scala.Any" || typeName == "scala.AnyRef" || typeName == "java.lang.Object"

          if (exactMatch || anyMatch) true
          else {
            // Try class hierarchy check
            try {
              val targetClass = Class.forName(typeName)
              targetClass.isInstance(other)
            } catch {
              case _: ClassNotFoundException => false
            }
          }
        } catch {
          case _: Exception => false
        }
    }
  }

  //==========================================================================
  // Try/Catch/Finally
  //==========================================================================

  private def interpretTry(block: Term, catches: List[CaseDef], finalizer: Option[Term])(using Env): AbstractAny = {
    def runFinalizer(): Unit = finalizer.foreach(f => eval(f))

    try {
      val result = eval(block)
      runFinalizer()
      result
    } catch {
      case e: Throwable =>
        // Try to match against catch cases
        val wrappedException = e match {
          case ie: InterpretedException => ie.underlying
          case _ => e
        }

        // Debug: print exception and patterns (disabled)
        // println(s"[DEBUG] Caught exception: ${wrappedException.getClass.getName}: ${wrappedException.getMessage}")
        // catches.foreach { c =>
        //   println(s"[DEBUG] Pattern: ${c.pattern.show(using Printer.TreeStructure)}")
        //   println(s"[DEBUG] Pattern matches: ${matchPatternForCatch(wrappedException, c.pattern)}")
        // }

        catches.find(c => matchPatternForCatch(wrappedException, c.pattern)) match {
          case Some(caseDef) =>
            val bindings = extractBindingsForCatch(wrappedException, caseDef.pattern)
            val result = eval(caseDef.rhs)(using summon[Env] ++ bindings)
            runFinalizer()
            result
          case None =>
            runFinalizer()
            throw e
        }
    }
  }

  /**
   * Special pattern matching for catch clauses.
   * Catch patterns have different structure than regular patterns.
   */
  private def matchPatternForCatch(exception: Throwable, pattern: Tree)(using Env): Boolean = pattern match {
    case Bind(_, inner) =>
      matchPatternForCatch(exception, inner)

    case Typed(_, tpt) =>
      isInstanceOfType(exception, tpt)

    case TypedOrTest(_, tpt) =>
      isInstanceOfType(exception, tpt)

    case Wildcard() =>
      true

    case _ =>
      // Fall back to regular pattern matching
      matchPattern(exception, pattern)
  }

  /**
   * Extract bindings for catch clause patterns.
   */
  private def extractBindingsForCatch(exception: Throwable, pattern: Tree)(using Env): Map[Symbol, LocalValue] = pattern match {
    case Bind(_, inner) =>
      val innerBindings = extractBindingsForCatch(exception, inner)
      innerBindings + (pattern.symbol -> LocalValue.valFrom(exception))

    case Typed(inner, _) =>
      extractBindingsForCatch(exception, inner)

    case TypedOrTest(inner, _) =>
      extractBindingsForCatch(exception, inner)

    case Wildcard() =>
      Map.empty

    case _ =>
      extractBindings(exception, pattern)
  }

  //==========================================================================
  // Return
  //==========================================================================

  private def interpretReturn(expr: Term)(using Env): AbstractAny = {
    val value = eval(expr)
    throw new ReturnException(value)
  }

  //==========================================================================
  // Closure (Lambda)
  //==========================================================================

  private def interpretClosure(meth: Term, tpt: Option[TypeRepr])(using Env): AbstractAny = {
    // meth is a reference to the method implementing the closure
    val methodSym = meth.symbol
    tastyLoader.loadMethodDef(methodSym) match {
      case Some(ddef) =>
        val params = ddef.termParamss.flatMap(_.params.map(_.symbol))
        new InterpretedClosure(ddef.rhs.get, params, summon[Env])
      case None =>
        // The method might be defined in the current block
        methodSym.tree match {
          case ddef: DefDef if ddef.rhs.isDefined =>
            val params = ddef.termParamss.flatMap(_.params.map(_.symbol))
            new InterpretedClosure(ddef.rhs.get, params, summon[Env])
          case _ =>
            throw new RuntimeException(s"Cannot find method ${methodSym.name} for closure")
        }
    }
  }

  private def interpretClosureDef(ddef: DefDef)(using Env): AbstractAny = {
    val params = ddef.termParamss.flatMap(_.params.map(_.symbol))
    ddef.rhs match {
      case Some(body) =>
        new InterpretedClosure(body, params, summon[Env])
      case None =>
        throw new RuntimeException(s"Closure ${ddef.name} has no body")
    }
  }

  //==========================================================================
  // This reference
  //==========================================================================

  private def interpretThis(qual: Option[String])(using Env): AbstractAny = {
    qual match {
      case Some(className) =>
        // Qualified this - e.g., Outer.this
        // Find the class symbol by name in the environment
        summon[Env].collectFirst {
          case (sym, local) if sym.isClassDef && sym.name == className => local.get
        }.getOrElse {
          throw new RuntimeException(s"Cannot find 'this' for class $className")
        }
      case None =>
        // Unqualified this - find the nearest enclosing class
        summon[Env].collectFirst {
          case (sym, local) if sym.isClassDef => local.get
        }.getOrElse {
          throw new RuntimeException("No 'this' in scope")
        }
    }
  }

  //==========================================================================
  // Primitive implementations
  //==========================================================================

  def interpretUnit(): AbstractAny = ()

  def interpretLiteral(const: Constant): Result = const.value

  def interpretIsInstanceOf(o: AbstractAny, tpt: TypeTree): Result = isInstanceOfType(o, tpt)

  def interpretAsInstanceOf(o: AbstractAny, tpt: TypeTree): Result = o  // Unchecked cast

  def interpretRepeated(elems: List[AbstractAny]): AbstractAny = elems.toSeq

  def interpretEqEq(x: AbstractAny, y: AbstractAny): AbstractAny = x == y

  def interpretPrivitiveLt(x: AbstractAny, y: AbstractAny): AbstractAny = numericOp(x, y)(_ < _)(_ < _)
  def interpretPrivitiveGt(x: AbstractAny, y: AbstractAny): AbstractAny = numericOp(x, y)(_ > _)(_ > _)
  def interpretPrivitiveLtEq(x: AbstractAny, y: AbstractAny): AbstractAny = numericOp(x, y)(_ <= _)(_ <= _)
  def interpretPrivitiveGtEq(x: AbstractAny, y: AbstractAny): AbstractAny = numericOp(x, y)(_ >= _)(_ >= _)
  def interpretPrivitivePlus(x: AbstractAny, y: AbstractAny): AbstractAny = numericOp(x, y)(_ + _)(_ + _)
  def interpretPrivitiveMinus(x: AbstractAny, y: AbstractAny): AbstractAny = numericOp(x, y)(_ - _)(_ - _)
  def interpretPrivitiveTimes(x: AbstractAny, y: AbstractAny): AbstractAny = numericOp(x, y)(_ * _)(_ * _)
  def interpretPrivitiveDiv(x: AbstractAny, y: AbstractAny): AbstractAny = numericOp(x, y)(_ / _)(_ / _)
  def interpretPrivitiveQuot(x: AbstractAny, y: AbstractAny): AbstractAny = integralOp(x, y)(_ / _)
  def interpretPrivitiveRem(x: AbstractAny, y: AbstractAny): AbstractAny = integralOp(x, y)(_ % _)

  private def numericOp[R](x: AbstractAny, y: AbstractAny)(intOp: (Long, Long) => R)(doubleOp: (Double, Double) => R): R = {
    (x, y) match {
      case (a: Int, b: Int) => intOp(a.toLong, b.toLong)
      case (a: Long, b: Long) => intOp(a, b)
      case (a: Int, b: Long) => intOp(a.toLong, b)
      case (a: Long, b: Int) => intOp(a, b.toLong)
      case (a: Double, b: Double) => doubleOp(a, b)
      case (a: Float, b: Float) => doubleOp(a.toDouble, b.toDouble)
      case (a: Int, b: Double) => doubleOp(a.toDouble, b)
      case (a: Double, b: Int) => doubleOp(a, b.toDouble)
      case _ => throw new RuntimeException(s"Cannot perform numeric operation on $x and $y")
    }
  }

  private def integralOp(x: AbstractAny, y: AbstractAny)(op: (Long, Long) => Long): AbstractAny = {
    (x, y) match {
      case (a: Int, b: Int) => op(a.toLong, b.toLong).toInt
      case (a: Long, b: Long) => op(a, b)
      case (a: Int, b: Long) => op(a.toLong, b)
      case (a: Long, b: Int) => op(a, b.toLong)
      case _ => throw new RuntimeException(s"Cannot perform integral operation on $x and $y")
    }
  }
}
