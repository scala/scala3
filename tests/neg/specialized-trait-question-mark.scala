//> using options -language:experimental.specializedTraits
inline trait Trait[S: Specialized]

def main =
    val x = new Trait[?] {} // error: Type argument must be fully defined
