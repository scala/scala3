//> using options -language:experimental.specializedTraits

// It's important to update the symbol infos for methods whose type interface changes due to specialization
// before we update any code that uses these symbols so that we type their uses' Apply nodes correctly.
// This only becomes clear when the user is in another object (and not a top-level main method)
// because otherwise the processing follows source code ordering.

inline trait Trait[T: Specialized]:
  def bar = "bar"

def foo(v: Trait[Int]) = v

object Test:
  def main(args: Array[String]): Unit =
    val a = new Trait[Int] {}
    foo(a).bar
