// scalajs: --skip

public class Test {
  public static void s(Object s) {
    System.out.println(s);
  }

  public static void statics(Class<?> c) {
    java.lang.reflect.Method[] ms = c.getDeclaredMethods();
    java.util.Arrays.sort(ms, (a, b) -> a.toString().compareTo(b.toString()));
    for (java.lang.reflect.Method a : ms) {
      if (java.lang.reflect.Modifier.isStatic(a.getModifiers()))
        s((a.isSynthetic() ? "synthetic " : "") + a);
    }
  }

  public static void main(String[] args) {
    s(D.foo(1).x());
    s(D.bar().trim());

    s(O.a());
    s(O.b());
    O.b_$eq(2);
    s(O.b());
    s(O.c());
    s(O.d());

    s(O.i());
    s(O.j());
    O.j_$eq(2);
    s(O.j());
    s(O.k());
    s(O.l());

    statics(D.class);
    statics(O.class);
  }
}
