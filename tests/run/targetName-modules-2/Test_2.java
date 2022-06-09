package alpha;

public class Test_2 {

  public static void main(String[] args) {
    assert new Outer$A(101).i() == 101;
    assert new Outer$A(Outer$A.$lessinit$greater$default$1()).i() == 101;
    assert new Outer$A(Outer$A$.MODULE$.$lessinit$greater$default$1()).i() == 101;
  }
}
