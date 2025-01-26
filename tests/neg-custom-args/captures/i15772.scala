

type Observe[T] = (T => Unit) -> Unit

def unsafe(cap: C^) = cap.bad()

def box1[T](v: T) : (T => Unit) -> Unit = {
  (fn: T => Unit) => fn(v)
}

def box2[T](v: T) : Observe[T] = {
  (fn: T => Unit) => fn(v)
}

class C(val arg: C^) {
  def bad() = println("I've gone bad!")
}

def main1(x: C^) : () -> Int =
  () =>
    val c : C^{x} = new C(x)  // error
    val boxed1 : ((C^) => Unit) -> Unit = box1(c)  // error
    boxed1((cap: C^) => unsafe(c))
    0

def main2(x: C^) : () -> Int =
  () =>
    val c : C^{x} = new C(x) // error
    val boxed2 : Observe[C^] = box2(c)  // error
    boxed2((cap: C^) => unsafe(c))
    0

def main3(x: C^) =
  def c : C^ = new C(x) // error separation
  val boxed2 : Observe[C]^ = box2(c) // error
  boxed2((cap: C^) => unsafe(c))
  0

trait File:
  def write(s: String): Unit

def main(io: Any^) =
  val sayHello: ((File^{io}) => Unit) = (file: File^{io}) => file.write("Hello World!\r\n")
  val filesList : List[File]^{io} = ???
  val x = () => filesList.foreach(sayHello)
  x: (() -> Unit) // error
