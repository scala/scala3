sealed trait Animal
object Cat extends Animal
object Dog extends Animal

type Mammal = Cat.type | Dog.type

class Test:
  def t1 =
    val mammals: List[Mammal] = ???
    val result = mammals.head
    val mammal: Mammal = result // was: Type Mismatch Error:
                                //      Found:    (result : Animal)
                                //      Required: Mammal
    ()
