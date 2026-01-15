import caps.{cap, Classifier, SharedCapability}

trait Read extends SharedCapability, Classifier

trait A extends Read

def weird(f: () ->{cap.only[Read]} Unit) = ???

def test(x: A^) =
  val g = () => println(x)
  weird(g)
