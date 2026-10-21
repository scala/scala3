//> using options -language:experimental.inlineTraits
inline trait A:
    def x = 1
trait B extends A // This is fine as long as A has no parameters.
