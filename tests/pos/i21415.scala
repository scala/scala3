//> using options -rewrite -source:3.4-migration
import scala.compiletime.testing.typeCheckErrors

def foo(arg: Int): Unit = ???

@main def Test =
  typeCheckErrors("Seq.empty[Int].foreach(foo.apply _)")
  typeCheckErrors("Seq.empty[Int].foreach(foo.apply _)")
