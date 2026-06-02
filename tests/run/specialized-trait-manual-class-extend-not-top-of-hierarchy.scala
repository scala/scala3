//> using options -language:experimental.specializedTraits

class Animal
class Mammal extends Animal
class Binturong extends Mammal

inline trait A[T: Specialized]

class B extends A[Binturong]

@main def Test =
    val traits = classOf[B].getInterfaces()    
    assert(traits.exists(cl => cl.getName() == "A$$sp$Animal"))
