package alpha;

public class Test_2 {

  public static void main(String[] args) {
    assert Outer$.A.foo() == 23;
    assert Outer$A$.MODULE$.foo() == 23;
  }
}
