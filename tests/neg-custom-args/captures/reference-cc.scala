import language.experimental.captureChecking
import scala.compiletime.uninitialized
import java.io.*
import language.experimental.saferExceptions


def usingLogFile[T](op: FileOutputStream^ => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result

trait LzyList[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: LzyList[A]^{this}
  def map[B](f: A => B): LzyList[B]^{f, this} = ???
object LzyList:
  def apply[A](xs: A*): LzyList[A] = ???

object LzyNil extends LzyList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???

final class LzyCons[+A](hd: A, tl: () => LzyList[A]^) extends LzyList[A]:
  private var forced = false
  private var cache: LzyList[A]^{this} = uninitialized
  private def force =
    if !forced then { cache = tl(); forced = true }
    cache

  def isEmpty = false
  def head = hd
  def tail: LzyList[A]^{this} = force
end LzyCons

class Cap extends caps.SharedCapability
def test(c: Cap) =
  class A:
    val x: A = this
    def f = println(c)

  val later = usingLogFile { file => () => file.write(0) } // error
  later() // crash

  val xs = usingLogFile: f => // error
    LzyList(1, 2, 3).map { x => f.write(x); x * x }

class LimitExceeded extends Exception

def testException =
  val limit = 10e+10
  def f(x: Double): Double throws LimitExceeded =
    if x < limit then x * x else throw LimitExceeded()

  def escaped(xs: Double*): (() => Double) throws LimitExceeded =
    try () => xs.map(f).sum  // error TODO improve error message
    catch case ex: LimitExceeded => () => -1
