import caps.*

class Ref extends Mutable:
  private var i = 0
  def get: Int = i
  update def put(x: Int): Unit = i = x

class A0[CS^]

class A1

def test0(): Unit =
  val r: Ref^ = Ref()
  val a: A0[{r}]^{r} = A0[{r}]  // error

def test1(): Unit =
  val r: Ref^ = Ref()
  val a: A1^{r} = A1()  // ok!