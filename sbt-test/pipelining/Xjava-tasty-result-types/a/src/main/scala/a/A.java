package a;

public class A {
  public static final String VALUE = "A";

  public <T> String add(T t) {
    return VALUE + t.toString();
  }
}
