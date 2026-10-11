//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits

inline trait Box[+T: Specialized]

def checkNothingInt(x: Box[Int]) = println("Good morning")
def checkNothingString(x: Box[String]) = println("Good afternoon")

@main def Test = 
    val x = new Box[Nothing] {}

    checkNothingInt(x)    // error: Illegal variance in specialized traits
    checkNothingString(x) // error: Illegal variance in specialized traits
