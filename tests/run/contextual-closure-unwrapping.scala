import scala.annotation.tailrec

import java.lang.management.ManagementFactory
import java.lang.management.RuntimeMXBean

import scala.jdk.CollectionConverters._

object Test {
  trait Txn {}
  type AtomicOp[Z] = Txn ?=> Z
  type VanillaAtomicOp[Z] = Txn => Z

  object AbortAndRetry extends scala.util.control.ControlThrowable("abort and retry") {}

  def beginTxn:Txn = new Txn {}

  @tailrec def retryN[Z](n:Int)(txn:AtomicOp[Z], i:Int = 0):Z = {
    try {
      given Txn = beginTxn
      val ret:Z = txn
      if(i < n) { throw AbortAndRetry }
      ret
    } catch {
      case AbortAndRetry => retryN(n)(txn, i + 1)
    }
  }

  @tailrec def safeRetryN[Z](n:Int)(txn:VanillaAtomicOp[Z], i:Int = 0):Z = {
    try {
      given Txn = beginTxn
      val ret:Z = txn.asInstanceOf[AtomicOp[Z]]
      if(i < n) { throw AbortAndRetry }
      ret
    } catch {
      case AbortAndRetry => safeRetryN(n)(txn, i + 1)
    }
  }

  object StackSize {
    def unapply(arg:String):Option[Int] = {
      Option(arg match {
        case s"-Xss$rest" => rest
        case s"-XX:ThreadStackSize=$rest" => rest
        case _ => null
      }).map{ rest =>
        val shift = (rest.toLowerCase.last match {
          case 'k' => 10
          case 'm' => 20
          case 'g' => 30
          case _ => 0
        })
        (if(shift > 0) {
          rest.dropRight(1)
        } else {
          rest
        }).toInt << shift
      }
    }
  }
  def main(args:Array[String]) = {
    val arguments = ManagementFactory.getRuntimeMXBean.getInputArguments
    // 64bit VM defaults to 1024K / 1M stack size, 32bit defaults to 320k
    // Use 1024 as upper bound.
    val maxStackSize:Int = arguments.asScala.reverseIterator.collectFirst{case StackSize(stackSize) => stackSize}.getOrElse(1 << 20)

    val testTxn:VanillaAtomicOp[Boolean] = {
      (txn:Txn) =>
        given Txn = txn
        true
    }

    Console.println(try {
      // maxStackSize is a good upper bound on linear stack growth
      // without assuming too much about frame size
      // (1 byte / frame is conservative even for a z80)
      retryN(maxStackSize)(testTxn.asInstanceOf[AtomicOp[Boolean]]) == safeRetryN(maxStackSize)(testTxn)
    } catch {
      case e:StackOverflowError =>
        Console.println(s"Exploded after ${e.getStackTrace.length} frames")
        false
    })
  }
}