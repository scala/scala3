import scala.annotation.tailrec
/**
 * Illustrates that abstracting over type arguments without triggering Ycheck failure is tricky
 *
 * go1.loop refers to type parameter of i321, and captures value f
 * if go1.loop will abstract over T it will need to cast f or will trigger a Ycheck failure.
 * One could decide to not abstract over type parameters in tail calls, but this leads us to go2 example
 *
 * In go2 we should abstract over i321.T, as we need to change it in recursive call.
 *
 * For now decision is such - we will abstract for top-level methods, but will not for inner ones.
 */

class i321[T >: Null <: AnyRef] {

  def go1(f: T => Int): Int = {
    @tailrec def loop(pending: T): Int = {
      val head1 = f(pending)
      loop(pending)
    }
    loop(null)
  }

  final def go2[U >: Null <: AnyRef](t: i321[U]): Int = t.go2(this)

}
