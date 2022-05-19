
package alpha;

public class Test_2 {

  public static void main(String[] args) {
    Alpha<String> a = new Outer$Bar();
    assert a.foo() == 1;
    assert a.bar("a").equals("aa");
    Alpha<String> aa = a.append(a);
  }
}
