import java.io.Serializable;

public class A_1 {
  public <T extends Object & Serializable> void foo(T x) {
    System.out.println("1");
  }

  public <T extends Cloneable & Serializable> void foo(T x) {
    System.out.println("2");
  }
}
