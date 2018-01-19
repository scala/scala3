public class Test {
  public Outer.Inner foo() {
    return null;
  }
}

class Outer<A> {
  class Inner {}
}
