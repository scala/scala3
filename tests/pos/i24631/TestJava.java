package example;

public abstract class TestJava<T> {
  public abstract T create(String foo);

  // Note that this is the method that's called from Scala code
  public T create() { return create(""); }

  public static class Concrete extends TestJava<String> {
    @Override public String create(String foo) { return foo; }
  }
}
