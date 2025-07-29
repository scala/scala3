import caps.{cap, Classifier, Capability}

trait Read extends Capability, Classifier
trait Write extends Capability, Classifier

trait A extends Read
trait B extends Write

def weird(f: () ->{cap.only[Read]} Unit) = ???

def test(x: A^, y: B^) =
  val g = () => println(x)
  weird(g) // ok
  val h = () => println(y)
  weird(h) // error
  val k = () => { println(x); println(y) }
  weird(k) // error