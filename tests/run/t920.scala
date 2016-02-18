object Test {
  trait A;
  trait Foo0 { def foo : A; }
  trait Baz extends Foo0;
  trait B extends A {
    def initialize = {
      trait Foo extends Test.Foo0 {
        def foo : B.this.type = B.this;
      }
      class baz extends Baz with Foo {
        override def toString = "baz"
      }
      Console.println(new baz);
    }
  }
  object bb extends B;
  def main(args : Array[String]) : Unit = {
    bb.initialize;
  }
}




