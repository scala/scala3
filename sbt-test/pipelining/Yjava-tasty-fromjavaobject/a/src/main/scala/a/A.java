// this test ensures that Object can accept Any from Scala
// see Definitions.FromJavaObjectSymbol
package a;

public class A {

  public static class Inner<T> extends Object {
    public Inner() {}

    public void meth1(T arg) {}
    public <U extends T> void meth2(U arg) {}
  }

  public static class Inner_sel<T> extends java.lang.Object {
    public Inner_sel() {}

    public void meth1(T arg) {}
    public <U extends T> void meth2(U arg) {}
  }

  // 1. At the top level:
  public void meth1(Object arg) {}
  public void meth1_sel(java.lang.Object arg) {}
  public <T> void meth2(T arg) {} // T implicitly extends Object

  // 2. In a class type parameter:
  public void meth3(scala.collection.immutable.List<Object> arg) {}
  public void meth3_sel(scala.collection.immutable.List<java.lang.Object> arg) {}
  public <T> void meth4(scala.collection.immutable.List<T> arg) {}

  // 3. As the type parameter of an array:
  public void meth5(Object[] arg) {}
  public void meth5_sel(java.lang.Object[] arg) {}
  public <T> void meth6(T[] arg) {}

  // 4. As the repeated argument of a varargs method:
  public void meth7(Object... args) {}
  public void meth7_sel(java.lang.Object... args) {}
  public <T> void meth8(T... args) {}
}
