public class A {
  public <T extends java.io.Serializable> void foo(T x) {}
  public <T extends Cloneable> void foo(T x) {}
}
