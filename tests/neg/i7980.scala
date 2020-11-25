trait Evidence[X]

trait Trait[X : Evidence]:
  def method(x : X) : X

given Evidence[Int] as ev = new Evidence[Int]{}
val crash : Trait[Int] = (x: Int) => x          // error
