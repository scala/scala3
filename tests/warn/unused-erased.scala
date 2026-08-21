//> using options -Wunused:all -language:experimental.erasedDefinitions

trait Ev:
  def foo: Unit = () // avoid Ev being treated as a marker trait

object Ev:
  def f(using erased ev: Ev): Int = answer     // no warn, erased using param is unused by design

  def g(erased x: Int): Int = answer           // no warn, erased explicit param is unused by design

  def h(using ev: Ev): Int = answer            // warn, non-erased using param is still checked

  def i(x: Int): Int = answer                  // warn, non-erased explicit param is still checked

trait EvErased extends compiletime.Erased:
  def foo: Unit = () // avoid Ev being treated as a marker trait


object EvErased:
  def f(using erased ev: EvErased): Int = answer     // no warn, explicitly erased

  def g(erased x: EvErased): Int = answer            // no warn, explicitly erased

  def h(using ev: EvErased): Int = answer            // no warn, type derives from Erased so param is implicitly erased

  def i(x: EvErased): Int = answer                   // no warn, type derives from Erased so param is implicitly erased


def localScope =
  val x = new Ev {}                           // warn, non-erased val is still checked
  erased val y = new Ev {}                    // warn, not affected
  val z = new EvErased {}                     // warn, not affected

def answer: Int = 42
