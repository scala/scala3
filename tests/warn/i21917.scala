//> using options -Wunused:imports

import Pet.Owner

class Dog(owner: Owner) extends Pet(owner) {
  import Pet.* // warn although unambiguous (i.e., it was disambiguated)
  //import Car.* // ambiguous

  def bark(): String = "bite"

  def this(owner: Owner, goodDog: Boolean) = {
    this(owner)
    if (goodDog) println(s"$owner's dog is a good boy")
  }

  val getOwner: Owner = owner
}

class Pet(val owner: Owner)

object Pet {
  class Owner
}

object Car {
  class Owner
}
