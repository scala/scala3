// this test ensures that Object can accept Any from Scala
// see Definitions.FromJavaObjectSymbol
package a;

import java.lang.Object;

// same signatures that reference Object explicitly from A.java, but with the java.lang.Object import
public class AImport {

  public static class Inner<T> extends Object {
    public Inner() {}

    public void meth1(T arg) {}
  }

  // 1. At the top level:
  public void meth1(Object arg) {}

  // 2. In a class type parameter:
  public void meth3(scala.collection.immutable.List<Object> arg) {}

  // 3. As the type parameter of an array:
  public void meth5(Object[] arg) {}

  // 4. As the repeated argument of a varargs method:
  public void meth7(Object... args) {}
}
