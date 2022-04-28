package alpha;

public class Test_2 {

  public static void main(String[] args) {
    assert A.foo() == 23;
    assert A$.MODULE$.foo() == 23;
  }
}
