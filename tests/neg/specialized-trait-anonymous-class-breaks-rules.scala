//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits

inline trait Baz[T: Specialized]:
    def foo = "hello world"

inline trait Foo[T: Specialized] extends Baz[T]:
    val bar = 10

inline trait Bar[T: Specialized]:
    val baz = 100

trait MyTrait[T](val x: T)

@main def main =
    val w = new Foo[Int] {} // ok

    val x = new Foo[Int] { // error: Anonymous classes acting as instances of Specialized traits may not have additional members; you can make a named object instead if you like.
        val y = 10
    }

    val y = new Foo[Int] with MyTrait(10) with Bar {} // error: Anonymous classes acting as instances of Specialized traits may not mix in other traits; you can make a named object instead if you like.

    val z = new Foo[Int] with Bar {} // error: Anonymous classes acting as instances of Specialized traits may not mix in other traits; you can make a named object instead if you like.
    
    val a = new Foo[Int] with Baz[Int] {} // ok: We have to allow this because it desugars to the same thing as the new Foo[Int] {} case; it will have the same behaviour when compiled;

    val b = new Baz[Int] with Foo[Int] {} // ok: We have to allow this because it desugars to the same thing as the new Foo[Int] {} case; it will have the same behaviour when compiled;
