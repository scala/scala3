import language.future // sepchecks on
import caps.consume

trait Cap

def useCap[X](x: X): (X -> Unit) -> Unit = ???

def test1(c: Cap^): Unit =
  val f: (Cap^{c} -> Unit) -> Unit = useCap[Cap^{c}](c)  // error

def test2(@consume c: Cap^, d: Cap^): Unit =
  def useCap1[X](x: X): (X => Unit) -> Unit = ???
  val f1: (Cap^{c} => Unit) ->{c} Unit = useCap1[Cap^{c}](c)  // ok

  def useCap2[X](x: X): (X ->{c} Unit) -> Unit = ???
  val f2: (Cap^{c} -> Unit) ->{c} Unit = useCap2[Cap^{c}](c)  // ok

  def useCap3[X](x: X): (X ->{d} Unit) -> Unit = ???
  val f3: (Cap^{c} -> Unit) => Unit = useCap3[Cap^{c}](c)  // error
