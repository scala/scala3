import language.experimental.captureChecking
import language.experimental.modularity
import caps.unsafe.untrackedCaptures

sealed class Region() extends caps.SharedCapability
final class MutRegion extends Region

sealed trait MyList[+A]:
  def head: A
object MyNil extends MyList[Nothing]:
  def head = throw new NoSuchElementException

class MyCons[A](val head: A, @untrackedCaptures private var _next: MyList[A]) extends MyList[A]:
  type SetterCapability <: Region
  def next: MyList[A] = _next
  def next_=(v: MyList[A])(using SetterCapability & MutRegion): Unit = _next = v

object MyCons:
  def apply[A](head: A, next: MyList[A])(using ev: Region)
    : MyCons[A] { type SetterCapability = ev.type } =
    new MyCons(head, next) { type SetterCapability = ev.type }

class MyListBuffer[A](tracked val heap: MutRegion = new MutRegion):
  @untrackedCaptures private var last0: MyCons[A] { type SetterCapability = heap.type } = null

  def assign(f: A): this.type =
    given this.heap.type = this.heap
    val v = MyCons(f, MyNil)
    last0.next = v // error
    this