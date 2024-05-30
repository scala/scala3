// scalajs: --skip

import scala.language.unsafeNulls
import java.lang.reflect.Method

object issue15402 {
  trait Named:
    def me: Named

  trait Foo extends Named:
    def me: Foo = this
    def foo(x: Named): Named

  trait Foo2 extends Foo
}

trait Adapter[T] extends Function1[T, Unit]
object issue15402b {
  def makeAdapter[T <: Number]: Adapter[T] = (arg: Number) => ()
  val adapterInteger = makeAdapter[Integer]
}

// Regression test based on Scala 2.13 stdlib
object collections {
  trait IterableOnce[A]
  trait Iterable[+A] extends IterableOps[A, Iterable, Iterable[A]]
  trait IterableFactory[+CC[_]] {
    def from[A](source: IterableOnce[A]): CC[A]
  }

  trait IterableOps[+A, +CC[_], +C] {
    def head: A = null.asInstanceOf[A]
    def tail: C = null.asInstanceOf[C]
  }
  trait Seq[+A] extends Iterable[A] with SeqOps[A, Seq, Seq[A]]
  object Seq extends SeqFactory.Delegate[Seq] {
    override def from[E](it: IterableOnce[E]): Seq[E] = ???
  }
  trait SeqOps[+A, +CC[_], +C] extends IterableOps[A, CC, C]
  trait SeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]]] extends IterableFactory[CC]
  object SeqFactory {
    class Delegate[CC[A] <: SeqOps[A, Seq, Seq[A]]] extends SeqFactory[CC] {
      def from[E](it: IterableOnce[E]): CC[E] = ???
    }
  }
  trait LinearSeq[+A]
      extends Seq[A]
      with LinearSeqOps[A, LinearSeq, LinearSeq[A]]
  trait LinearSeqOps[
      +A,
      +CC[X] <: LinearSeq[X],
      +C <: LinearSeq[A] with LinearSeqOps[A, CC, C]
  ] extends SeqOps[A, CC, C] {
    override def head: A
    override def tail: C
  }
}


// Regression test based on Scala 2.13 stdlib
// andThen for Nil was generated with Function1 instead of Function1<Nothing, A] type
trait Func1[-T1, +R] {
  def apply(v1: T1): R
  def andThen[A](g: Func1[R, A]): Func1[T1, A] = ???
}
trait PartialFunc[-A, +B] extends Func1[A, B] { self =>
  override def andThen[C](k: Func1[B, C]): PartialFunc[A, C] = ???
}
trait Collection[+A] extends PartialFunc[Int, A] {
  override def apply(v: Int): A = ???
}
case object EmptyCollection extends Collection[Nothing]

object play {
  trait WSCookie
  trait StandaloneWSRequest {
    type Self <: StandaloneWSRequest { type Self = StandaloneWSRequest.this.Self }
    def withCookies(cookies: WSCookie*): Self
    def addCookies(cookies: WSCookie*): Self = ???
  }

  trait WSRequest extends StandaloneWSRequest {
    override type Self = WSRequest
    override def addCookies(cookies: WSCookie*): Self
    override def withCookies(cookie: WSCookie*): Self
  }
}

object Test {
  def flagsString(m: java.lang.reflect.Method) = {
    val str = List(
      Option.when(m.isBridge)("<bridge>"),
      Option.when(m.isSynthetic)("<synthetic>")
    ).flatten.mkString(" ")

    if (str.isEmpty()) "" else " " + str
  }

  def show(clazz: Class[_], filter: Method => Boolean = _ => true): Unit = {
    print(clazz.toString + " {")
    clazz.getMethods
      .filter(filter)
      .sortBy(x => (x.getName, x.isBridge, x.toString))
      .foreach { m =>
        print("\n  " + m + flagsString(m))
        if (m.toString() != m.toGenericString) {
          print("\n  - generic: " + m.toGenericString)
        }
      }
    println("\n}")
    println("")
  }

  def main(args: Array[String]): Unit = {
    List(
      classOf[issue15402.Named],
      classOf[issue15402.Foo],
      classOf[issue15402.Foo2],
    ).foreach(show(_, _.getName() == "me"))

    List(
      classOf[Adapter[?]],
      issue15402b.adapterInteger.getClass()
    ).foreach(show(_, _.getName() == "apply"))

    List(
      classOf[collections.LinearSeqOps[?, ?, ?]],
      classOf[collections.LinearSeq[?]],
      classOf[collections.Iterable[?]],
      classOf[collections.IterableOps[?, ?, ?]],
      Class.forName("collections$Seq")
    ).foreach(show(_, m => List("head", "tail", "from").contains(m.getName())))

    show(Class.forName("EmptyCollection"), _.getName() == "andThen")

    show(classOf[play.WSRequest])
  }
}
