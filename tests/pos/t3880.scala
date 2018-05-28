abstract class Bar[+B] {
}
final class C1[+B] extends Bar[B] {
  private[this] def g(x: C1[B]): Unit = ()

  // this method is fine: notice that it allows the call to g,
  // which requires C1[B], even though we matched on C1[_].
  // (That is good news, but is sound only because C1 is final; see #3989
  // and compare with i3989a.scala.
  private[this] def f1(x: Bar[B]): Unit = x match {
    case x: C1[_] => g(x)
  }
  // this one crashes.
  private[this] def f2(x: Bar[B]): Unit = x match {
    case x: C1[_] => f2(x)
  }
}
