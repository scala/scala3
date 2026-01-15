class Toto
class Tata extends Toto

extension (x: Toto)
  def add(y: Toto): Toto = Toto()
extension (x: Tata)
  def add(y: Toto): Toto = { println("this is what we want called!") ; Toto() }
  def add(y: Tata): Tata = Tata()

@main def Test = println:
  val a: Tata = Tata()
  val b: Toto = Toto()
  a.add(b) // error
