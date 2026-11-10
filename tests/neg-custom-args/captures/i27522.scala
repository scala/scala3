import language.experimental.separationChecking
import caps.*
def take(consume f: () => Unit): Unit = ()
class B extends Mutable:
  def use(): Unit = ()
  def run(): Unit =
    take(() => this.use())  // error, as expected
  def run1(): Unit =
    List(1).foreach(i => take(() => this.use()))  // error