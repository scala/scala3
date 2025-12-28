package dotty.tools.dotc
package quoted

import scala.collection.mutable

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.TreeInfo
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.SrcPos

/**
 * A TASTy-based interpreter that interprets method bodies from TASTy trees
 * instead of using JVM reflection when TASTy is available.
 *
 * This is the foundation for cross-platform macro execution (Scala-Native, Scala-JS)
 * where JVM reflection is not available.
 *
 * The interpreter extends the existing JVM-reflection based Interpreter and
 * overrides methods to use TASTy interpretation when possible, falling back
 * to JVM reflection for code without TASTy bodies available.
 */
class TastyBasedInterpreter(pos: SrcPos, classLoader0: ClassLoader)(using Context)
    extends Interpreter(pos, classLoader0):

  import Interpreter.*

  // Instrumentation counters for tracking TASTy vs JVM usage
  private var tastyMethodCalls = 0
  private var jvmMethodCalls = 0
  private var tastyModuleAccess = 0
  private var jvmModuleAccess = 0
  private var tastyNewInstance = 0
  private var jvmNewInstance = 0
  private var intrinsicCalls = 0

  //==========================================================================
  // Output capture for program execution
  //==========================================================================

  /** Buffer to capture println output during interpretation */
  private val outputBuffer = new StringBuilder()

  /** Get captured output */
  def getCapturedOutput: String = outputBuffer.toString()

  /** Clear the output buffer */
  def clearOutput(): Unit = outputBuffer.clear()

  //==========================================================================
  // Intrinsics system - pure implementations without JVM reflection
  //==========================================================================

  /** Intrinsic implementations for common stdlib methods */
  private type IntrinsicFn = (List[Object]) => Object

  private lazy val intrinsics: Map[String, IntrinsicFn] = Map(
    // Console/Predef println variants
    "scala.Predef$.println" -> intrinsicPrintln,
    "scala.Predef.println" -> intrinsicPrintln,
    "scala.Console$.println" -> intrinsicPrintln,
    "scala.Console.println" -> intrinsicPrintln,
    "scala.io.StdIn$.println" -> intrinsicPrintln,

    // Print without newline
    "scala.Predef$.print" -> intrinsicPrint,
    "scala.Predef.print" -> intrinsicPrint,
    "scala.Console$.print" -> intrinsicPrint,
    "scala.Console.print" -> intrinsicPrint,

    // String operations
    "java.lang.String.length" -> { args => args.head.asInstanceOf[String].length.asInstanceOf[Object] },
    "java.lang.String.charAt" -> { args =>
      val s = args.head.asInstanceOf[String]
      val i = args(1).asInstanceOf[Int]
      s.charAt(i).asInstanceOf[Object]
    },
    "java.lang.String.substring" -> { args =>
      args.size match
        case 2 =>
          val s = args.head.asInstanceOf[String]
          val start = args(1).asInstanceOf[Int]
          s.substring(start).asInstanceOf[Object]
        case 3 =>
          val s = args.head.asInstanceOf[String]
          val start = args(1).asInstanceOf[Int]
          val end = args(2).asInstanceOf[Int]
          s.substring(start, end).asInstanceOf[Object]
        case _ => throw new RuntimeException("Invalid substring args")
    },
    "java.lang.String.concat" -> { args =>
      val s1 = args.head.asInstanceOf[String]
      val s2 = args(1).asInstanceOf[String]
      s1.concat(s2).asInstanceOf[Object]
    },
    "java.lang.String.trim" -> { args => args.head.asInstanceOf[String].trim.asInstanceOf[Object] },
    "java.lang.String.toLowerCase" -> { args => args.head.asInstanceOf[String].toLowerCase.asInstanceOf[Object] },
    "java.lang.String.toUpperCase" -> { args => args.head.asInstanceOf[String].toUpperCase.asInstanceOf[Object] },
    "java.lang.String.isEmpty" -> { args => args.head.asInstanceOf[String].isEmpty.asInstanceOf[Object] },
    "java.lang.String.contains" -> { args =>
      val s = args.head.asInstanceOf[String]
      val sub = args(1).asInstanceOf[CharSequence]
      s.contains(sub).asInstanceOf[Object]
    },
    "java.lang.String.startsWith" -> { args =>
      val s = args.head.asInstanceOf[String]
      val prefix = args(1).asInstanceOf[String]
      s.startsWith(prefix).asInstanceOf[Object]
    },
    "java.lang.String.endsWith" -> { args =>
      val s = args.head.asInstanceOf[String]
      val suffix = args(1).asInstanceOf[String]
      s.endsWith(suffix).asInstanceOf[Object]
    },
    "java.lang.String.replace" -> { args =>
      val s = args.head.asInstanceOf[String]
      val oldStr = args(1).asInstanceOf[CharSequence]
      val newStr = args(2).asInstanceOf[CharSequence]
      s.replace(oldStr, newStr).asInstanceOf[Object]
    },
    "java.lang.String.split" -> { args =>
      val s = args.head.asInstanceOf[String]
      val regex = args(1).asInstanceOf[String]
      s.split(regex).asInstanceOf[Object]
    },
    "java.lang.String.toCharArray" -> { args => args.head.asInstanceOf[String].toCharArray.asInstanceOf[Object] },
    "java.lang.String.indexOf" -> { args =>
      val s = args.head.asInstanceOf[String]
      args(1) match
        case ch: java.lang.Character => s.indexOf(ch.charValue).asInstanceOf[Object]
        case ch: java.lang.Integer => s.indexOf(ch.intValue).asInstanceOf[Object]
        case str: String => s.indexOf(str).asInstanceOf[Object]
        case _ => s.indexOf(args(1).toString).asInstanceOf[Object]
    },

    // Integer/primitive operations
    "scala.Int.toString" -> { args => args.head.toString.asInstanceOf[Object] },
    "scala.Long.toString" -> { args => args.head.toString.asInstanceOf[Object] },
    "scala.Double.toString" -> { args => args.head.toString.asInstanceOf[Object] },
    "scala.Float.toString" -> { args => args.head.toString.asInstanceOf[Object] },
    "scala.Boolean.toString" -> { args => args.head.toString.asInstanceOf[Object] },
    "scala.Char.toString" -> { args => args.head.toString.asInstanceOf[Object] },

    // Any/AnyRef operations
    "java.lang.Object.toString" -> { args => args.head.toString.asInstanceOf[Object] },
    "java.lang.Object.hashCode" -> { args => args.head.hashCode.asInstanceOf[Object] },
    "java.lang.Object.equals" -> { args => args.head.equals(args(1)).asInstanceOf[Object] },
    "scala.Any.toString" -> { args => args.head.toString.asInstanceOf[Object] },
    "scala.Any.hashCode" -> { args => args.head.hashCode.asInstanceOf[Object] },
    "scala.Any.==" -> { args => (args.head == args(1)).asInstanceOf[Object] },
    "scala.Any.!=" -> { args => (args.head != args(1)).asInstanceOf[Object] },

    // List operations
    "scala.collection.immutable.List.head" -> { args => args.head.asInstanceOf[List[?]].head.asInstanceOf[Object] },
    "scala.collection.immutable.List.tail" -> { args => args.head.asInstanceOf[List[?]].tail.asInstanceOf[Object] },
    "scala.collection.immutable.List.isEmpty" -> { args => args.head.asInstanceOf[List[?]].isEmpty.asInstanceOf[Object] },
    "scala.collection.immutable.List.nonEmpty" -> { args => args.head.asInstanceOf[List[?]].nonEmpty.asInstanceOf[Object] },
    "scala.collection.immutable.List.length" -> { args => args.head.asInstanceOf[List[?]].length.asInstanceOf[Object] },
    "scala.collection.immutable.List.size" -> { args => args.head.asInstanceOf[List[?]].size.asInstanceOf[Object] },
    "scala.collection.immutable.List.reverse" -> { args => args.head.asInstanceOf[List[?]].reverse.asInstanceOf[Object] },
    "scala.collection.immutable.List.headOption" -> { args => args.head.asInstanceOf[List[?]].headOption.asInstanceOf[Object] },
    "scala.collection.immutable.List.lastOption" -> { args => args.head.asInstanceOf[List[?]].lastOption.asInstanceOf[Object] },
    "scala.collection.immutable.List.last" -> { args => args.head.asInstanceOf[List[?]].last.asInstanceOf[Object] },
    "scala.collection.immutable.List.init" -> { args => args.head.asInstanceOf[List[?]].init.asInstanceOf[Object] },
    "scala.collection.immutable.List.take" -> { args =>
      val list = args.head.asInstanceOf[List[?]]
      val n = args(1).asInstanceOf[Int]
      list.take(n).asInstanceOf[Object]
    },
    "scala.collection.immutable.List.drop" -> { args =>
      val list = args.head.asInstanceOf[List[?]]
      val n = args(1).asInstanceOf[Int]
      list.drop(n).asInstanceOf[Object]
    },
    "scala.collection.immutable.List.mkString" -> { args =>
      args.size match
        case 1 => args.head.asInstanceOf[List[?]].mkString.asInstanceOf[Object]
        case 2 => args.head.asInstanceOf[List[?]].mkString(args(1).asInstanceOf[String]).asInstanceOf[Object]
        case 4 =>
          val list = args.head.asInstanceOf[List[?]]
          list.mkString(args(1).asInstanceOf[String], args(2).asInstanceOf[String], args(3).asInstanceOf[String]).asInstanceOf[Object]
        case _ => throw new RuntimeException("Invalid mkString args")
    },
    "scala.collection.immutable.List.contains" -> { args =>
      val list = args.head.asInstanceOf[List[?]]
      list.contains(args(1)).asInstanceOf[Object]
    },
    "scala.collection.immutable.List.apply" -> { args =>
      val list = args.head.asInstanceOf[List[?]]
      val idx = args(1).asInstanceOf[Int]
      list(idx).asInstanceOf[Object]
    },
    "scala.collection.immutable.List.+:" -> { args =>
      val elem = args.head
      val list = args(1).asInstanceOf[List[Any]]
      (elem +: list).asInstanceOf[Object]
    },
    "scala.collection.immutable.List.:+" -> { args =>
      val list = args.head.asInstanceOf[List[Any]]
      val elem = args(1)
      (list :+ elem).asInstanceOf[Object]
    },
    "scala.collection.immutable.List.:::" -> { args =>
      val list1 = args.head.asInstanceOf[List[Any]]
      val list2 = args(1).asInstanceOf[List[Any]]
      (list1 ::: list2).asInstanceOf[Object]
    },
    "scala.collection.immutable.List.map" -> intrinsicListMap,
    "scala.collection.immutable.List.flatMap" -> intrinsicListFlatMap,
    "scala.collection.immutable.List.filter" -> intrinsicListFilter,
    "scala.collection.immutable.List.filterNot" -> intrinsicListFilterNot,
    "scala.collection.immutable.List.foreach" -> intrinsicListForeach,
    "scala.collection.immutable.List.foldLeft" -> intrinsicListFoldLeft,
    "scala.collection.immutable.List.foldRight" -> intrinsicListFoldRight,
    "scala.collection.immutable.List.reduce" -> intrinsicListReduce,
    "scala.collection.immutable.List.find" -> intrinsicListFind,
    "scala.collection.immutable.List.exists" -> intrinsicListExists,
    "scala.collection.immutable.List.forall" -> intrinsicListForall,
    "scala.collection.immutable.List.count" -> intrinsicListCount,
    "scala.collection.immutable.List.zip" -> { args =>
      val list1 = args.head.asInstanceOf[List[Any]]
      val list2 = args(1).asInstanceOf[Iterable[Any]]
      list1.zip(list2).asInstanceOf[Object]
    },
    "scala.collection.immutable.List.zipWithIndex" -> { args =>
      args.head.asInstanceOf[List[?]].zipWithIndex.asInstanceOf[Object]
    },

    // Nil object
    "scala.collection.immutable.Nil$.head" -> { _ => throw new NoSuchElementException("head of empty list") },
    "scala.collection.immutable.Nil$.tail" -> { _ => throw new UnsupportedOperationException("tail of empty list") },
    "scala.collection.immutable.Nil$.isEmpty" -> { _ => true.asInstanceOf[Object] },

    // :: (cons) operations
    "scala.collection.immutable.::$.apply" -> { args =>
      val head = args.head
      val tail = args(1).asInstanceOf[List[Any]]
      (head :: tail).asInstanceOf[Object]
    },
    "scala.collection.immutable.::.head" -> { args => args.head.asInstanceOf[::[?]].head.asInstanceOf[Object] },
    "scala.collection.immutable.::.tail" -> { args => args.head.asInstanceOf[::[?]].tail.asInstanceOf[Object] },

    // Option operations
    "scala.Option.isEmpty" -> { args => args.head.asInstanceOf[Option[?]].isEmpty.asInstanceOf[Object] },
    "scala.Option.nonEmpty" -> { args => args.head.asInstanceOf[Option[?]].nonEmpty.asInstanceOf[Object] },
    "scala.Option.isDefined" -> { args => args.head.asInstanceOf[Option[?]].isDefined.asInstanceOf[Object] },
    "scala.Option.get" -> { args => args.head.asInstanceOf[Option[?]].get.asInstanceOf[Object] },
    "scala.Option.getOrElse" -> { args =>
      val opt = args.head.asInstanceOf[Option[Any]]
      val default = args(1) match
        case f: Function0[?] => f()
        case v => v
      opt.getOrElse(default).asInstanceOf[Object]
    },
    "scala.Option.orElse" -> { args =>
      val opt = args.head.asInstanceOf[Option[Any]]
      val alternative = args(1) match
        case f: Function0[?] => f().asInstanceOf[Option[Any]]
        case o: Option[?] => o.asInstanceOf[Option[Any]]
        case _ => None
      opt.orElse(alternative).asInstanceOf[Object]
    },
    "scala.Option.map" -> intrinsicOptionMap,
    "scala.Option.flatMap" -> intrinsicOptionFlatMap,
    "scala.Option.filter" -> intrinsicOptionFilter,
    "scala.Option.foreach" -> intrinsicOptionForeach,
    "scala.Option.fold" -> intrinsicOptionFold,
    "scala.Option.exists" -> intrinsicOptionExists,
    "scala.Option.forall" -> intrinsicOptionForall,
    "scala.Option.contains" -> { args =>
      val opt = args.head.asInstanceOf[Option[Any]]
      opt.contains(args(1)).asInstanceOf[Object]
    },
    "scala.Option.toList" -> { args => args.head.asInstanceOf[Option[?]].toList.asInstanceOf[Object] },

    // Some operations
    "scala.Some.get" -> { args => args.head.asInstanceOf[Some[?]].get.asInstanceOf[Object] },
    "scala.Some.isEmpty" -> { _ => false.asInstanceOf[Object] },
    "scala.Some$.apply" -> { args => Some(args.head).asInstanceOf[Object] },

    // None operations
    "scala.None$.get" -> { _ => throw new NoSuchElementException("None.get") },
    "scala.None$.isEmpty" -> { _ => true.asInstanceOf[Object] },

    // Tuple operations
    "scala.Tuple2._1" -> { args => args.head.asInstanceOf[(Any, Any)]._1.asInstanceOf[Object] },
    "scala.Tuple2._2" -> { args => args.head.asInstanceOf[(Any, Any)]._2.asInstanceOf[Object] },
    "scala.Tuple3._1" -> { args => args.head.asInstanceOf[(Any, Any, Any)]._1.asInstanceOf[Object] },
    "scala.Tuple3._2" -> { args => args.head.asInstanceOf[(Any, Any, Any)]._2.asInstanceOf[Object] },
    "scala.Tuple3._3" -> { args => args.head.asInstanceOf[(Any, Any, Any)]._3.asInstanceOf[Object] },
    "scala.Tuple2$.apply" -> { args => (args.head, args(1)).asInstanceOf[Object] },
    "scala.Tuple3$.apply" -> { args => (args.head, args(1), args(2)).asInstanceOf[Object] },

    // Math operations
    "scala.math.package$.abs" -> { args =>
      args.head match
        case i: java.lang.Integer => math.abs(i.intValue).asInstanceOf[Object]
        case l: java.lang.Long => math.abs(l.longValue).asInstanceOf[Object]
        case d: java.lang.Double => math.abs(d.doubleValue).asInstanceOf[Object]
        case f: java.lang.Float => math.abs(f.floatValue).asInstanceOf[Object]
        case _ => throw new RuntimeException("Invalid abs argument")
    },
    "scala.math.package$.max" -> { args =>
      (args.head, args(1)) match
        case (a: java.lang.Integer, b: java.lang.Integer) => math.max(a.intValue, b.intValue).asInstanceOf[Object]
        case (a: java.lang.Long, b: java.lang.Long) => math.max(a.longValue, b.longValue).asInstanceOf[Object]
        case (a: java.lang.Double, b: java.lang.Double) => math.max(a.doubleValue, b.doubleValue).asInstanceOf[Object]
        case _ => throw new RuntimeException("Invalid max arguments")
    },
    "scala.math.package$.min" -> { args =>
      (args.head, args(1)) match
        case (a: java.lang.Integer, b: java.lang.Integer) => math.min(a.intValue, b.intValue).asInstanceOf[Object]
        case (a: java.lang.Long, b: java.lang.Long) => math.min(a.longValue, b.longValue).asInstanceOf[Object]
        case (a: java.lang.Double, b: java.lang.Double) => math.min(a.doubleValue, b.doubleValue).asInstanceOf[Object]
        case _ => throw new RuntimeException("Invalid min arguments")
    },
    "scala.math.package$.sqrt" -> { args => math.sqrt(args.head.asInstanceOf[Double]).asInstanceOf[Object] },
    "scala.math.package$.pow" -> { args => math.pow(args.head.asInstanceOf[Double], args(1).asInstanceOf[Double]).asInstanceOf[Object] },
    "scala.math.package$.floor" -> { args => math.floor(args.head.asInstanceOf[Double]).asInstanceOf[Object] },
    "scala.math.package$.ceil" -> { args => math.ceil(args.head.asInstanceOf[Double]).asInstanceOf[Object] },
    "scala.math.package$.round" -> { args =>
      args.head match
        case d: java.lang.Double => math.round(d.doubleValue).asInstanceOf[Object]
        case f: java.lang.Float => math.round(f.floatValue).asInstanceOf[Object]
        case _ => throw new RuntimeException("Invalid round argument")
    },

    // Array operations
    "scala.Array.length" -> { args => args.head.asInstanceOf[Array[?]].length.asInstanceOf[Object] },
    "scala.Array.apply" -> { args =>
      val arr = args.head.asInstanceOf[Array[Any]]
      val idx = args(1).asInstanceOf[Int]
      arr(idx).asInstanceOf[Object]
    },
    "scala.Array.update" -> { args =>
      val arr = args.head.asInstanceOf[Array[Any]]
      val idx = args(1).asInstanceOf[Int]
      val value = args(2)
      arr(idx) = value
      ().asInstanceOf[Object]
    },
    "scala.Array.toList" -> { args => args.head.asInstanceOf[Array[?]].toList.asInstanceOf[Object] },

    // Predef utilities
    "scala.Predef$.identity" -> { args => args.head },
    "scala.Predef$.implicitly" -> { args => args.head },
    "scala.Predef$.???" -> { _ => throw new NotImplementedError("an implementation is missing") },
    "scala.Predef$.require" -> { args =>
      val cond = args.head.asInstanceOf[Boolean]
      if !cond then throw new IllegalArgumentException("requirement failed")
      ().asInstanceOf[Object]
    },
    "scala.Predef$.assert" -> { args =>
      val cond = args.head.asInstanceOf[Boolean]
      if !cond then throw new AssertionError("assertion failed")
      ().asInstanceOf[Object]
    },

    // StringContext for string interpolation
    "scala.StringContext.s" -> intrinsicStringInterpolation,
    "scala.StringContext$.apply" -> { args =>
      val parts = args.head.asInstanceOf[Seq[String]]
      StringContext(parts*)
    },
  )

  /** Intrinsic: println with newline */
  private val intrinsicPrintln: IntrinsicFn = { args =>
    val msg = if args.isEmpty then "" else String.valueOf(args.head)
    outputBuffer.append(msg).append("\n")
    if logEnabled then
      println(s"[TastyInterpreter] println: $msg")
    ().asInstanceOf[Object]
  }

  /** Intrinsic: print without newline */
  private val intrinsicPrint: IntrinsicFn = { args =>
    val msg = if args.isEmpty then "" else String.valueOf(args.head)
    outputBuffer.append(msg)
    if logEnabled then
      print(s"[TastyInterpreter] print: $msg")
    ().asInstanceOf[Object]
  }

  //==========================================================================
  // List higher-order intrinsics
  //==========================================================================

  private val intrinsicListMap: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val f = args(1).asInstanceOf[Any => Any]
    list.map(f).asInstanceOf[Object]
  }

  private val intrinsicListFlatMap: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val f = args(1).asInstanceOf[Any => IterableOnce[Any]]
    list.flatMap(f).asInstanceOf[Object]
  }

  private val intrinsicListFilter: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    list.filter(p).asInstanceOf[Object]
  }

  private val intrinsicListFilterNot: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    list.filterNot(p).asInstanceOf[Object]
  }

  private val intrinsicListForeach: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val f = args(1).asInstanceOf[Any => Unit]
    list.foreach(f)
    ().asInstanceOf[Object]
  }

  private val intrinsicListFoldLeft: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val z = args(1)
    val op = args(2).asInstanceOf[(Any, Any) => Any]
    list.foldLeft(z)(op).asInstanceOf[Object]
  }

  private val intrinsicListFoldRight: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val z = args(1)
    val op = args(2).asInstanceOf[(Any, Any) => Any]
    list.foldRight(z)(op).asInstanceOf[Object]
  }

  private val intrinsicListReduce: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val op = args(1).asInstanceOf[(Any, Any) => Any]
    list.reduce(op).asInstanceOf[Object]
  }

  private val intrinsicListFind: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    list.find(p).asInstanceOf[Object]
  }

  private val intrinsicListExists: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    list.exists(p).asInstanceOf[Object]
  }

  private val intrinsicListForall: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    list.forall(p).asInstanceOf[Object]
  }

  private val intrinsicListCount: IntrinsicFn = { args =>
    val list = args.head.asInstanceOf[List[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    list.count(p).asInstanceOf[Object]
  }

  //==========================================================================
  // Option higher-order intrinsics
  //==========================================================================

  private val intrinsicOptionMap: IntrinsicFn = { args =>
    val opt = args.head.asInstanceOf[Option[Any]]
    val f = args(1).asInstanceOf[Any => Any]
    opt.map(f).asInstanceOf[Object]
  }

  private val intrinsicOptionFlatMap: IntrinsicFn = { args =>
    val opt = args.head.asInstanceOf[Option[Any]]
    val f = args(1).asInstanceOf[Any => Option[Any]]
    opt.flatMap(f).asInstanceOf[Object]
  }

  private val intrinsicOptionFilter: IntrinsicFn = { args =>
    val opt = args.head.asInstanceOf[Option[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    opt.filter(p).asInstanceOf[Object]
  }

  private val intrinsicOptionForeach: IntrinsicFn = { args =>
    val opt = args.head.asInstanceOf[Option[Any]]
    val f = args(1).asInstanceOf[Any => Unit]
    opt.foreach(f)
    ().asInstanceOf[Object]
  }

  private val intrinsicOptionFold: IntrinsicFn = { args =>
    val opt = args.head.asInstanceOf[Option[Any]]
    val ifEmpty = args(1) match
      case f: Function0[?] => f()
      case v => v
    val f = args(2).asInstanceOf[Any => Any]
    opt.fold(ifEmpty)(f).asInstanceOf[Object]
  }

  private val intrinsicOptionExists: IntrinsicFn = { args =>
    val opt = args.head.asInstanceOf[Option[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    opt.exists(p).asInstanceOf[Object]
  }

  private val intrinsicOptionForall: IntrinsicFn = { args =>
    val opt = args.head.asInstanceOf[Option[Any]]
    val p = args(1).asInstanceOf[Any => Boolean]
    opt.forall(p).asInstanceOf[Object]
  }

  //==========================================================================
  // String interpolation intrinsic
  //==========================================================================

  private val intrinsicStringInterpolation: IntrinsicFn = { args =>
    val sc = args.head.asInstanceOf[StringContext]
    val parts = sc.parts
    val values = args.tail
    val sb = new StringBuilder()
    val partsIter = parts.iterator
    val valuesIter = values.iterator
    while partsIter.hasNext do
      sb.append(partsIter.next())
      if valuesIter.hasNext then
        sb.append(String.valueOf(valuesIter.next()))
    sb.toString.asInstanceOf[Object]
  }

  /** Check if a method has an intrinsic implementation */
  private def hasIntrinsic(sym: Symbol): Boolean =
    val fullName = sym.fullName.toString
    intrinsics.contains(fullName)

  /** Call an intrinsic method */
  private def callIntrinsic(sym: Symbol, args: List[Object]): Object =
    val fullName = sym.fullName.toString
    intrinsicCalls += 1
    if logEnabled then
      println(s"[TastyInterpreter] Intrinsic call: $fullName")
    intrinsics(fullName)(args)

  /** Get instrumentation stats - useful for debugging */
  def getStats: String =
    s"""TastyBasedInterpreter Stats:
       |  Method calls: TASTy=$tastyMethodCalls, JVM=$jvmMethodCalls, Intrinsic=$intrinsicCalls
       |  Module access: TASTy=$tastyModuleAccess, JVM=$jvmModuleAccess
       |  New instances: TASTy=$tastyNewInstance, JVM=$jvmNewInstance
       |  Output captured: ${outputBuffer.length} chars""".stripMargin

  //==========================================================================
  // Public API for program execution
  //==========================================================================

  /**
   * Execute a static method by symbol.
   * This is the public entry point for ExecutionEngine.
   *
   * @param moduleClass The module class containing the method
   * @param methodSym The method symbol to execute
   * @param args The arguments to pass
   * @return The result of the method call
   */
  def executeMethod(moduleClass: Symbol, methodSym: Symbol, args: List[Object]): Object =
    interpretedStaticMethodCall(moduleClass, methodSym, args)

  /**
   * Execute a method body directly from its DefDef tree.
   * Useful for executing main methods.
   *
   * @param mainDef The DefDef of the method to execute
   * @param args The arguments to pass
   * @return The result of the method execution
   */
  def executeMainMethod(mainDef: tpd.DefDef, args: Array[String]): Object =
    val moduleSym = mainDef.symbol.owner
    val moduleClass = if moduleSym.is(Module) then moduleSym.moduleClass else moduleSym
    interpretedStaticMethodCall(moduleClass, mainDef.symbol, List(args.asInstanceOf[Object]))

  /** Enable detailed logging (set via -Ylog:interpreter) */
  private def logEnabled: Boolean = ctx.settings.Ylog.value.contains("interpreter")

  private def logTastyMethod(sym: Symbol): Unit =
    tastyMethodCalls += 1
    if logEnabled then
      println(s"[TastyInterpreter] TASTy method: ${sym.fullName}")

  private def logJvmMethod(sym: Symbol): Unit =
    jvmMethodCalls += 1
    if logEnabled then
      // Log with additional info about why TASTy wasn't available
      val reason = sym.defTree match
        case ddef: DefDef if ddef.rhs.isEmpty => "abstract method"
        case _: DefDef => "unknown"
        case _ => "no defTree"
      println(s"[TastyInterpreter] JVM fallback method: ${sym.fullName} (reason: $reason)")

  private def logTastyModule(sym: Symbol): Unit =
    tastyModuleAccess += 1
    if logEnabled then
      println(s"[TastyInterpreter] TASTy module: ${sym.fullName}")

  private def logJvmModule(sym: Symbol): Unit =
    jvmModuleAccess += 1
    if logEnabled then
      println(s"[TastyInterpreter] JVM fallback module: ${sym.fullName}")

  private def logTastyNew(sym: Symbol): Unit =
    tastyNewInstance += 1
    if logEnabled then
      println(s"[TastyInterpreter] TASTy new: ${sym.fullName}")

  private def logJvmNew(sym: Symbol): Unit =
    jvmNewInstance += 1
    if logEnabled then
      println(s"[TastyInterpreter] JVM fallback new: ${sym.fullName}")

  /** Exception for non-local returns */
  private class ReturnException(val value: Object) extends Exception

  /** Interpreted object instance - replaces JVM reflection Proxy */
  private class InterpretedInstance(
    val classSym: Symbol,
    val fields: mutable.Map[Symbol, Object]
  ):
    override def toString: String = s"InterpretedInstance(${classSym.fullName})"

  /** Interpreted closure with captured environment */
  private class InterpretedClosure(
    val body: Tree,
    val params: List[Symbol],
    val capturedEnv: Env
  ):
    override def toString: String = s"InterpretedClosure(${params.map(_.name).mkString(", ")})"

  /** Local method definition stored in environment for later invocation */
  private class LocalMethodDef(val ddef: DefDef):
    override def toString: String = s"LocalMethodDef(${ddef.name})"

  //==========================================================================
  // Check if TASTy bodies are available
  //==========================================================================

  /** Check if a symbol has a TASTy body available */
  private def hasTastyBody(sym: Symbol): Boolean =
    sym.defTree match
      case ddef: DefDef => !ddef.rhs.isEmpty
      case _ => false

  /** Check if a class has TASTy definition available */
  private def hasTastyClass(classSym: Symbol): Boolean =
    classSym.defTree.isInstanceOf[TypeDef]

  //==========================================================================
  // Override JVM reflection methods to try TASTy interpretation first
  //==========================================================================

  /**
   * Override static method call to try intrinsics, then TASTy interpretation, then JVM fallback.
   * Priority: Intrinsics > TASTy > JVM reflection
   */
  override protected def interpretedStaticMethodCall(moduleClass: Symbol, fn: Symbol, args: List[Object]): Object =
    if hasIntrinsic(fn) then
      callIntrinsic(fn, args)
    else if hasTastyBody(fn) then
      logTastyMethod(fn)
      interpretMethodFromTasty(fn, args)(using emptyEnv)
    else
      logJvmMethod(fn)
      super.interpretedStaticMethodCall(moduleClass, fn, args)

  /**
   * Override module access to try TASTy interpretation first.
   * Falls back to JVM reflection if TASTy is not available.
   */
  override protected def interpretModuleAccess(fn: Symbol): Object =
    val moduleClass = fn.moduleClass
    if hasTastyClass(moduleClass) then
      logTastyModule(moduleClass)
      interpretModuleFromTasty(moduleClass)
    else
      logJvmModule(moduleClass)
      super.interpretModuleAccess(fn)

  /**
   * Override new instance creation to try TASTy interpretation first.
   * Falls back to JVM reflection if TASTy is not available.
   */
  override protected def interpretNew(fn: Symbol, args: List[Object]): Object =
    val classSym = fn.owner
    if hasTastyClass(classSym) then
      logTastyNew(classSym)
      interpretNewFromTasty(classSym, fn, args)(using emptyEnv)
    else
      logJvmNew(classSym)
      super.interpretNew(fn, args)

  //==========================================================================
  // TASTy-based interpretation methods
  //==========================================================================

  /**
   * Interpret a method call from its TASTy body.
   */
  private def interpretMethodFromTasty(methodSym: Symbol, args: List[Object])(using env: Env): Object =
    methodSym.defTree match
      case ddef: DefDef if !ddef.rhs.isEmpty =>
        val paramSymbols = ddef.termParamss.flatten.map(_.symbol)
        val paramBindings = paramSymbols.zip(args).toMap
        try
          interpretTree(ddef.rhs)(using env ++ paramBindings)
        catch
          case ret: ReturnException => ret.value
      case _ =>
        throw StopInterpretation(em"Cannot interpret method ${methodSym.fullName}: no TASTy body available", pos)

  /**
   * Interpret module initialization from TASTy.
   */
  private def interpretModuleFromTasty(moduleClass: Symbol): Object =
    // For now, fall back to reflection for modules
    // TODO: Implement full module initialization from TASTy
    super.loadModule(moduleClass)

  /**
   * Interpret object construction from TASTy.
   */
  private def interpretNewFromTasty(classSym: Symbol, ctorSym: Symbol, args: List[Object])(using env: Env): Object =
    classSym.defTree match
      case tdef: TypeDef =>
        tdef.rhs match
          case template: Template =>
            // Create instance and initialize fields
            val instance = new InterpretedInstance(classSym, mutable.Map.empty)

            // Bind constructor parameters
            val ctorDef = template.constr
            val paramSymbols = ctorDef.termParamss.flatten.map(_.symbol)
            val paramBindings = paramSymbols.zip(args).toMap

            // Store constructor args as fields (for case classes)
            paramSymbols.zip(args).foreach { case (sym, value) =>
              instance.fields(sym) = value
            }

            // Initialize fields from template body
            val instanceEnv = env ++ paramBindings + (classSym -> instance)
            template.body.foreach {
              case vdef: ValDef if !vdef.rhs.isEmpty && !vdef.symbol.is(ParamAccessor) =>
                val value = interpretTree(vdef.rhs)(using instanceEnv)
                instance.fields(vdef.symbol) = value
              case _ => // Skip methods and other definitions
            }

            instance

          case _ =>
            // Fall back to JVM reflection
            super.interpretNew(ctorSym, args)
      case _ =>
        // Fall back to JVM reflection
        super.interpretNew(ctorSym, args)

  //==========================================================================
  // Override interpretTree to handle additional tree types
  //==========================================================================

  override protected def interpretTree(tree: Tree)(using env: Env): Object =
    tree match
      // Handle calls to local methods stored in the environment
      case Call(fn, args) if env.get(fn.symbol).exists(_.isInstanceOf[LocalMethodDef]) =>
        val localMethod = env(fn.symbol).asInstanceOf[LocalMethodDef]
        val argValues = args.flatten.map(interpretTree)
        invokeLocalMethod(localMethod, argValues)

      // Handle If expressions
      case If(cond, thenp, elsep) =>
        if interpretTree(cond).asInstanceOf[Boolean] then
          interpretTree(thenp)
        else
          interpretTree(elsep)

      // Handle While loops (note: WhileDo in tpd)
      case WhileDo(cond, body) =>
        while interpretTree(cond).asInstanceOf[Boolean] do
          interpretTree(body)
        ().asInstanceOf[Object]

      // Handle Match expressions
      case Match(selector, cases) =>
        interpretMatch(selector, cases)

      // Handle Try/Catch/Finally
      case Try(block, catches, finalizer) =>
        interpretTry(block, catches, finalizer)

      // Handle Return (both regular and labeled returns)
      case Return(expr, from) =>
        handleReturn(expr, from)

      // Handle This references
      case tree: This =>
        interpretThis(tree)

      // Handle Assign
      case Assign(lhs, rhs) =>
        // For now, skip assignment (side effects in interpreted code)
        // TODO: Implement proper assignment handling
        interpretTree(rhs)

      // Handle Block with Import, TypeDef statements (but NOT closure definitions)
      // closureDef pattern (Block(DefDef :: Nil, Closure)) should be handled by parent
      case block @ Block(stats, expr) if needsLocalDefHandling(stats, expr) =>
        interpretBlockWithLocalDefs(stats, expr)

      // Handle Import - skip at runtime, imports are compile-time only
      case Import(_, _) =>
        ().asInstanceOf[Object]

      // Handle standalone TypeDef - skip at runtime
      case _: TypeDef =>
        ().asInstanceOf[Object]

      // Handle Labeled blocks (used for complex control flow like return-from-match)
      case Labeled(bind, expr) =>
        interpretLabeled(bind.symbol, expr)

      // Handle Inlined code blocks
      case Inlined(call, bindings, expansion) =>
        interpretInlined(bindings, expansion)

      // Handle SeqLiteral and JavaSeqLiteral
      case SeqLiteral(elems, elemtpt) =>
        val values = elems.map(interpretTree)
        values.toArray.asInstanceOf[Object]

      case _ =>
        // Fall back to parent implementation
        super.interpretTree(tree)

  //==========================================================================
  // Match expression interpretation
  //==========================================================================

  private def interpretMatch(selector: Tree, cases: List[CaseDef])(using env: Env): Object =
    val scrutinee = interpretTree(selector)
    findMatchingCase(scrutinee, cases) match
      case Some((caseDef, bindings)) =>
        val newEnv = env ++ bindings
        interpretTree(caseDef.body)(using newEnv)
      case None =>
        throw new MatchError(s"No case matched: $scrutinee")

  private def findMatchingCase(scrutinee: Object, cases: List[CaseDef])(using env: Env): Option[(CaseDef, Map[Symbol, Object])] =
    cases.iterator.flatMap { caseDef =>
      matchPattern(scrutinee, caseDef.pat) match
        case Some(bindings) =>
          val guardPasses = caseDef.guard match
            case guard if guard.isEmpty => true
            case guard =>
              val guardEnv = env ++ bindings
              interpretTree(guard)(using guardEnv).asInstanceOf[Boolean]
          if guardPasses then Some((caseDef, bindings))
          else None
        case None => None
    }.nextOption()

  private def matchPattern(scrutinee: Object, pattern: Tree)(using Env): Option[Map[Symbol, Object]] =
    pattern match
      // Wildcard pattern: `_`
      case Ident(nme.WILDCARD) =>
        Some(Map.empty)

      // Bind pattern: `x @ pat`
      case Bind(name, inner) =>
        matchPattern(scrutinee, inner).map(_ + (pattern.symbol -> scrutinee))

      // Literal pattern
      case Literal(const) =>
        if scrutinee == const.value then Some(Map.empty) else None

      // Type pattern: `_: T`
      case Typed(Ident(nme.WILDCARD), tpt) =>
        if isInstanceOfType(scrutinee, tpt.tpe) then Some(Map.empty) else None

      // Typed pattern: `pat: T`
      case Typed(inner, tpt) =>
        if isInstanceOfType(scrutinee, tpt.tpe) then matchPattern(scrutinee, inner)
        else None

      // Alternative pattern: `pat1 | pat2`
      case Alternative(alts) =>
        alts.iterator.flatMap(matchPattern(scrutinee, _)).nextOption()

      // Extractor pattern: `Some(x)`, `List(a, b)`
      case UnApply(fun, implicits, patterns) =>
        interpretExtractor(scrutinee, fun, implicits) match
          case Some(extracted) => matchUnapplyResult(extracted, patterns)
          case None => None
          case true if patterns.isEmpty => Some(Map.empty)
          case false => None
          case _ => None

      // Module pattern: `None`, `Nil`
      case tree: Ident if tree.symbol.is(Module) =>
        val moduleValue = interpretModuleAccess(tree.symbol)
        if scrutinee == moduleValue then Some(Map.empty) else None

      // Variable pattern
      case tree: Ident =>
        Some(Map(tree.symbol -> scrutinee))

      case _ =>
        None

  private def matchUnapplyResult(extracted: Any, patterns: List[Tree])(using Env): Option[Map[Symbol, Object]] =
    extracted match
      case tuple: Product if patterns.size > 1 =>
        val elements = (0 until tuple.productArity).map(tuple.productElement)
        if elements.size == patterns.size then
          patterns.zip(elements).foldLeft(Option(Map.empty[Symbol, Object])) {
            case (Some(acc), (pat, elem)) =>
              matchPattern(elem.asInstanceOf[Object], pat).map(acc ++ _)
            case (None, _) => None
          }
        else None
      case single if patterns.size == 1 =>
        matchPattern(single.asInstanceOf[Object], patterns.head)
      case () if patterns.isEmpty =>
        Some(Map.empty)
      case _ =>
        None

  private def interpretExtractor(scrutinee: Object, fun: Tree, implicits: List[Tree])(using Env): Any =
    // Get the unapply method and try to call it
    val unapplySym = fun.symbol
    val ownerSym = unapplySym.owner

    // Handle common stdlib extractors
    ownerSym.fullName.toString match
      case "scala.Some$" =>
        scrutinee match
          case s: Some[?] => Some(s.get)
          case _ => None

      case "scala.None$" =>
        if scrutinee == None then Some(()) else None

      case s if s.contains("$colon$colon") || s.endsWith("::") =>
        scrutinee match
          case head :: tail => Some((head, tail))
          case _ => None

      case _ =>
        // Try to interpret the unapply method from TASTy
        if hasTastyBody(unapplySym) then
          interpretMethodFromTasty(unapplySym, scrutinee :: implicits.map(interpretTree(_)))
        else
          // Fall back to JVM reflection
          tryReflectionUnapply(scrutinee, fun, implicits)

  private def tryReflectionUnapply(scrutinee: Object, fun: Tree, implicits: List[Tree])(using Env): Any =
    try
      val ownerSym = fun.symbol.owner
      val moduleClass = if ownerSym.is(Module) then ownerSym.moduleClass else ownerSym
      val inst = loadModule(moduleClass)
      val clazz = inst.getClass
      val method = clazz.getMethod("unapply", classOf[Object])
      method.invoke(inst, scrutinee)
    catch
      case _: Exception => None

  //==========================================================================
  // Type checking for pattern matching
  //==========================================================================

  private def isInstanceOfType(value: Object, tpe: Type): Boolean =
    val typeSymbol = tpe.typeSymbol
    val typeName = typeSymbol.fullName.toString

    value match
      case null =>
        !tpe.derivesFrom(defn.AnyValClass)

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
      case _: String =>
        typeName == "java.lang.String" || typeName == "scala.Predef.String" ||
        typeName == "scala.Any" || typeName == "scala.AnyRef"

      case _: List[?] =>
        tpe.derivesFrom(defn.ListClass) || tpe.derivesFrom(defn.SeqClass) || typeName == "scala.Any"
      case _: Option[?] =>
        tpe.derivesFrom(defn.OptionClass) || typeName == "scala.Any"

      case inst: InterpretedInstance =>
        inst.classSym.typeRef <:< tpe || typeName == "scala.Any"

      case e: Throwable =>
        try
          val targetClass = Class.forName(typeName)
          targetClass.isInstance(e)
        catch
          case _: ClassNotFoundException => false

      case other =>
        try
          val targetClass = Class.forName(typeName)
          targetClass.isInstance(other)
        catch
          case _: ClassNotFoundException => false

  //==========================================================================
  // Try/Catch/Finally
  //==========================================================================

  private def interpretTry(block: Tree, catches: List[CaseDef], finalizer: Tree)(using env: Env): Object =
    def runFinalizer(): Unit =
      if !finalizer.isEmpty then interpretTree(finalizer)

    try
      val result = interpretTree(block)
      runFinalizer()
      result
    catch
      case e: ReturnException =>
        runFinalizer()
        throw e
      case e: Throwable =>
        catches.iterator.flatMap { caseDef =>
          matchPattern(e.asInstanceOf[Object], caseDef.pat) match
            case Some(bindings) =>
              val newEnv = env ++ bindings
              Some(interpretTree(caseDef.body)(using newEnv))
            case None => None
        }.nextOption() match
          case Some(result) =>
            runFinalizer()
            result
          case None =>
            runFinalizer()
            throw e

  //==========================================================================
  // This reference
  //==========================================================================

  private def interpretThis(tree: This)(using env: Env): Object =
    // Look for 'this' in the environment (bound when entering a method on an instance)
    val qual = tree.qual
    val result = env.collectFirst {
      case (sym, value) if sym.isClass =>
        if qual.isEmpty then value
        else if qual.name == sym.name then value
        else null
    }
    result.flatMap(Option(_)).getOrElse(throw new RuntimeException("No 'this' in scope"))

  //==========================================================================
  // Labeled blocks (for complex control flow)
  //==========================================================================

  /** Exception used to implement non-local return from labeled blocks */
  private class LabeledReturnException(val label: Symbol, val value: Object) extends Exception

  private def interpretLabeled(label: Symbol, expr: Tree)(using env: Env): Object =
    try
      interpretTree(expr)
    catch
      case ret: LabeledReturnException if ret.label == label =>
        ret.value

  /** Override Return to also handle labeled returns */
  private def handleReturn(expr: Tree, from: Tree)(using env: Env): Object =
    val value = interpretTree(expr)
    from match
      case Ident(name) if from.symbol.exists =>
        // Return to a labeled block
        throw new LabeledReturnException(from.symbol, value)
      case _ =>
        // Regular method return
        throw new ReturnException(value)

  //==========================================================================
  // Inlined code handling
  //==========================================================================

  private def interpretInlined(bindings: List[MemberDef], expansion: Tree)(using env: Env): Object =
    // Process bindings to create a new environment
    val newEnv = bindings.foldLeft(env) { (accEnv, binding) =>
      binding match
        case vdef: ValDef if !vdef.rhs.isEmpty =>
          accEnv.updated(vdef.symbol, interpretTree(vdef.rhs)(using accEnv))
        case ddef: DefDef =>
          // Store local method definition for later invocation
          accEnv.updated(ddef.symbol, new LocalMethodDef(ddef))
        case _ => accEnv
    }
    // Interpret the expansion in the new environment
    interpretTree(expansion)(using newEnv)

  //==========================================================================
  // Block with local definitions (DefDef, TypeDef, Import)
  //==========================================================================

  /** Check if a block needs special local def handling (not a closure definition) */
  private def needsLocalDefHandling(stats: List[Tree], expr: Tree): Boolean =
    // Don't handle closure definitions (Block(DefDef :: Nil, Closure)) - let parent handle
    val isClosureDef = stats match
      case (ddef: DefDef) :: Nil => expr match
        case closure: Closure => ddef.symbol == closure.meth.symbol
        case _ => false
      case _ => false

    !isClosureDef && stats.exists(s =>
      s.isInstanceOf[Import] ||
      s.isInstanceOf[TypeDef] ||
      (s.isInstanceOf[DefDef] && !s.asInstanceOf[DefDef].symbol.isAnonymousFunction)
    )

  private def interpretBlockWithLocalDefs(stats: List[Tree], expr: Tree)(using env: Env): Object =
    val newEnv = stats.foldLeft(env) { (accEnv, stat) =>
      stat match
        case vdef: ValDef =>
          // Value definition - evaluate and store
          accEnv.updated(vdef.symbol, interpretTree(vdef.rhs)(using accEnv))
        case ddef: DefDef =>
          // Local method definition - store for later invocation
          accEnv.updated(ddef.symbol, new LocalMethodDef(ddef))
        case _: TypeDef =>
          // Type definition - compile-time only, skip
          accEnv
        case _: Import =>
          // Import - compile-time only, skip
          accEnv
        case other =>
          // Other statements - try to interpret for side effects
          try
            interpretTree(other)(using accEnv)
          catch
            case _: StopInterpretation => // Ignore unexpected trees in block statements
          accEnv
    }
    interpretTree(expr)(using newEnv)

  /** Invoke a local method definition stored in the environment */
  private def invokeLocalMethod(localMethod: LocalMethodDef, args: List[Object])(using env: Env): Object =
    val ddef = localMethod.ddef
    val paramSymbols = ddef.termParamss.flatten.map(_.symbol)
    val paramBindings = paramSymbols.zip(args).toMap
    try
      interpretTree(ddef.rhs)(using env ++ paramBindings)
    catch
      case ret: ReturnException => ret.value

end TastyBasedInterpreter
