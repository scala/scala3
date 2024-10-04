//> using options -Wunused:all

import Pet.Owner

class Dog(owner: Owner) extends Pet(owner) {
  import Pet.*

  def bark(): String = "bite"

  def this(owner: Owner, goodDog: Boolean) =
    this(owner)
    if goodDog then println(s"$owner's dog is a good boy")
  
  //val getOwner: Owner = owner
}

trait Pet(owner: Owner)

object Pet {
  class Owner
}
