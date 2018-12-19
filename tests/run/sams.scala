trait X { def foo(x: Int): Int; def bar = foo(2) }

trait T {
  var f = 2
  def foo(x: Int): Int
}

trait U extends T

trait V extends Exception { def foo(x: Int): Int }

trait Y extends X {
  def baz = super.bar
}

trait Z {
  def foo(x: Int): Int; 42
}
trait ZZ extends Z

abstract class C {
  def foo(x: Int): Int
  trait I { def foo(x: Int): Int }
}

object Test {


  def main(args: Array[String]): Unit = {
    {
      val x: X = (x: Int) => 2  // should be a closure
      assert(x.foo(100) == 2)
    }

    {
      val t: T = (x: Int) => 2   // needs to be an anonymous class because of defined field
      assert(t.foo(200) == 2)
    }

    {
      val u: U = (x: Int) => 2   // needs to be an anonymous class because of inherited field
      assert(u.foo(100) == 2)
    }

    {
      val v: V = (x: Int) => 2   // needs to be an anonymous class because the trait extends a non-object class
      assert(v.foo(100) == 2)
    }

    {
      val y: Y = (x: Int) => 2   // needs to be an anonymous class because of super accessor
      assert(y.foo(100) == 2)
    }

    {
      val z: Z = (x: Int) => 2   // needs to be an anonymous class because trait has initialization code
      val zz: ZZ = (x: Int) => 3   // needs to be an anonymous class becaiuse trait has initialization code

      assert(z.foo(100) == 2)
      assert(zz.foo(100) == 3)
    }

    {
      val c: C = (x: Int) => 2   // needs to be an anonymous class because C is not a trait
      val ci: c.I = (x: Int) => 2 // needs to be an anonymous class because it needs an outer pointer
      
      assert(c.foo(100) == 2)
      assert(ci.foo(100) == 2)
    }

    // Partial functions
    val pf: PartialFunction[Int, Int] = {
      case 1 => 1
      case 2 => 2
    }

    val qf: PartialFunction[(Int, String), Int] = {
      case (1, "abc") => 1
      case _ => 2
    } 

    val rf: PartialFunction[(Int, AnyRef), Int] = {
      case (_: Int, _: String) => 1
      case _ => 2
    }

    val sf: PartialFunction[Any, Int] = {
      case x: String if x == "abc" => 1
    }
 
    assert(pf(1) == 1)
    assert(qf((1, "abc")) == 1)
    assert(rf(1, "hello") == 1)
    assert(sf("abc") == 1)

    // With null
    {
      val f: (Int => Int)|Null = (x: Int) => 2
      assert(f.nn(100) == 2)
    }

    {
      val pf: PartialFunction[Int, Int]|Null = {
        case 1 => 1
      }
      assert(pf.nn(1) == 1)
    }

    {
      val x: X|Null = (x: Int) => 2  
      assert(x.nn.foo(100) == 2)
    }

    {
      val t: T|Null = (x: Int) => 2   
      assert(t.nn.foo(200) == 2)
    }

    {
      val u: U|Null = (x: Int) => 2   
      assert(u.nn.foo(100) == 2)
    }

    {
      val v: V|Null = (x: Int) => 2   
      assert(v.nn.foo(100) == 2)
    }

    {
      val y: Y|Null = (x: Int) => 2  
      assert(y.nn.foo(100) == 2)
    }

    {
      val z: Z|Null = (x: Int) => 2   
      val zz: ZZ|Null = (x: Int) => 3   

      assert(z.nn.foo(100) == 2)
      assert(zz.nn.foo(100) == 3)
    }

    {
      val c: C|Null = (x: Int) => 2   
      assert(c.nn.foo(100) == 2)
    }
  }
}

