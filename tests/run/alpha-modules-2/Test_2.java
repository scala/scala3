package alpha;

public class Test_2 {

  public static void main(String[] args) {
    assert new A(101).i() == 101;
    assert new A(A.$lessinit$greater$default$1()).i() == 101;
    assert new A(A$.MODULE$.$lessinit$greater$default$1()).i() == 101;
  }
}
