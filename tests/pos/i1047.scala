package hello

object world extends App {
  println("hello dotty!")

  trait AnimalPackage {
    type Animal <: AnimalU
    type AnimalU = { val age: Int }
    def newAnimal(a: AnimalU): Animal
    def newSubAnimal[T](a: AnimalU & T): Animal & T
  }
  val p: AnimalPackage = new AnimalPackage { p =>
    type Animal = AnimalU
    override def newAnimal(a: AnimalU): Animal = a
    override def newSubAnimal[T](a: AnimalU & T): Animal & T = a
  }
  val lambda: p.Animal = p.newAnimal(new { val age = 1 })
  trait CatPackage { pc =>
    type Cat <: p.Animal & pc.CatDelta
    type CatDelta = { val meow: Int }
    type CatU = p.AnimalU & pc.CatDelta
    def newCat(c: CatU): Cat
    def newSubCat[T](c: CatU & T): Cat & T
  }
  val pc: CatPackage = new CatPackage { pc =>
    type Cat = p.Animal & pc.CatDelta
    def newCat(c: CatU): Cat = p.newSubAnimal[pc.CatDelta](c)
    def newSubCat[T](c: CatU & T): Cat & T = p.newSubAnimal[pc.CatDelta & T](c)
  }
  val felix: pc.Cat = pc.newCat(new { val age = 1; val meow = 2 })
}
