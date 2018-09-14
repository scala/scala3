// A test which exercises both param forwarding and explicit self types,
// so param forwarder definitions will get symbolic references.
// It leads to tricky situations which manifest themselves by pickle
// failures. Before pickling, a param accessor still had
// the value type (which is wrong), when reading back the
// pickled info, this type is then the correct ExprType.
// Fixed by adapating references oto param forwarders in ParamForwarding.scala
// Without the symblolic reference, this error was somehow masked by
// the fact that the reference cache was already updated to the
// good info.
package test

class Base(val x: Int)

abstract class Middle(x: Int) extends Base(x) { self: Sub =>

  def f(y: Int): Int = x + y

}

class Sub extends Middle(2) {

  override def f(x: Int) = x

}

