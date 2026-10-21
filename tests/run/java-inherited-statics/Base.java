package pkg;

public class Base implements Consts {
  public static String base() { return "Base.base"; }

  public static class Nested {
    public static String nested() { return "Base.Nested.nested"; }
  }
}
