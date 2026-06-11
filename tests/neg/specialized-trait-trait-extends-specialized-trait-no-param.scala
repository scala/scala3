//> using options -language:experimental.specializedTraits
inline trait Foo[T: Specialized]

trait Bar extends Foo[Int]: // error: Specialized traits may not be extended by ordinary traits. They may only be extended by classes, objects or inline/specialized traits.
    def myMethod = "Hello I am a method"
