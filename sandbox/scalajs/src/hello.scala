// package hello

// import scala.scalajs.js

// trait MyTrait {
//   val x = 5
//   def foo(y: Int) = x
// }

// object HelloWorld extends MyTrait {
//   def main(args: Array[String]): Unit = {
//     println("hello dotty.js!")
//     println(foo(4))
//   }
// }

object hello:
  trait World

object bye:
  trait World

object bla:
  object longname:
    object otherlongname:
      trait HelloWorldFactory
import bla.longname.otherlongname.HelloWorldFactory

def method(
    p1: hello.World,
    p2: bye.World,
    p3: hello.World,
    p4: HelloWorldFactory,
    p5: HelloWorldFactory,
    p6: HelloWorldFactory,
    p7: HelloWorldFactory
) = 1

def method(
    p1: hello.World,
    p1_1: HelloWorldFactory,
    p2: hello.World,
    p3: hello.World,
    p4: HelloWorldFactory
) = 1

def method(
    p1: String,
    p1_1: HelloWorldFactory,
    p2: hello.World,
    p3: hello.World,
    p4: HelloWorldFactory
) = 1


@main def test = 
  import hello.World
  method(new World, new World, 25, new World, new HelloWorldFactory)
