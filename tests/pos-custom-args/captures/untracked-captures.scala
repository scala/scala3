import caps.unsafe.untrackedCaptures

class LL[+A] private (@untrackedCaptures lazyState: () => LL.State[A]^):
  private val res = lazyState()


object LL:

  private trait State[+A]
  private object State:
    object Empty extends State[Nothing]

  private def newLL[A](state: () => State[A]^): LL[A]^{state} = ???

  private def sCons[A](hd: A, tl: LL[A]^): State[A]^{tl} = ???

  def filterImpl[A](ll: LL[A]^, p: A => Boolean): LL[A]^{ll, p} =
      // DO NOT REFERENCE `ll` ANYWHERE ELSE, OR IT WILL LEAK THE HEAD
      @untrackedCaptures var restRef: LL[A]^{ll} = ll  // restRef is captured by closure arg to newLL, so A is not recognized as parametric

      val cl = () =>
        var elem: A = null.asInstanceOf[A]
        var found   = false
        var rest    = restRef  // Without untracked captures a type ascription would be needed here
                               // because the compiler tries to keep track of lazyState in refinements
                               // of LL and gets confused (c.f Setup.addCaptureRefinements)

        while !found do
          found   = p(elem)
          rest    = rest
          restRef = rest
        val res = if found then sCons(elem, filterImpl(rest, p)) else State.Empty
        ??? : State[A]^{ll, p}
      val nll = newLL(cl)
      nll
