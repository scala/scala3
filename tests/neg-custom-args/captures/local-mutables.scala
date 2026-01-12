import caps.*

class Ref extends Mutable:
  var cur = 0
  def get(): Int = cur
  update def set(x: Int) = cur = x

def test1(): Unit =
  val r = Ref()
  val getter = () => r.get()
  val _: () -> Int = getter // error
  val _: () ->{r.rd} Int = getter // ok
  val setter = () => r.set(1)
  val _: () -> Unit = setter // error
  val _: () ->{r} Unit = setter // ok

  class C:
    def get(): Int = r.get()
    def set(x: Int) = r.set(x)
  class D extends Mutable:
    def get(): Int = r.get()
    def set(x: Int) = r.set(x) // error
  class E:
    def get(): Int = r.get()
  val c = C()
  val _: C^{r} = c
  val d = D()
  val _: D^{r, any} = d
  val e = E()
  val _: E^{r.rd} = e

def test2(): Unit =
  var r: Int = 0
  val getter = () => r
  val _: () -> Int = getter // error
  val _: () ->{r.rd} Int = getter // ok

  val setter = () => r = 1
  val _: () -> Unit = setter // error
  val _: () ->{r} Unit = setter // ok

  class C:
    def get(): Int = r
    def set(x: Int) = r = x  // ok
  class D extends Mutable:
    def get(): Int = r
    def set(x: Int) = r = x  // error
  class E:
    def get(): Int = r
  val c = C()
  val _: C^{r} = c
  val d = D()
  val _: D^{r, any} = d
  val e = E()
  val _: E^{r.rd} = e
