import caps.{cap, Classifier, Capability}

trait Read extends Capability, Classifier

trait A extends Read

def weird(f: () ->{cap.only[Read]} Unit) = ???

def test(x: A^) =
  val g = () => println(x)
  weird(g)
