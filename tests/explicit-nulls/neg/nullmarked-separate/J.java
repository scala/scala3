package marked;

import org.jspecify.annotations.*;

// Null-marked through its package
public class J {
  public String field = "";
  public @Nullable String nullableField;

  public J(String s) {}

  @NullUnmarked
  public J(String s, String t) {}

  public String get() { return ""; }
  public @Nullable String getNullable() { return null; }
  public void set(String s) {}
  public void setNullable(@Nullable String s) {}

  @NullUnmarked
  public String unmarkedGet() { return ""; }

  // Both annotations behave like neither: still null-marked through the package
  @NullMarked @NullUnmarked
  public String bothGet() { return ""; }

  public static String staticGet() { return ""; }
  public static @Nullable String staticGetNullable() { return null; }

  public static class Nested {
    public String get() { return ""; }
    public static String staticGet() { return ""; }
  }

  public class Inner {
    public String get() { return ""; }
  }

  @NullUnmarked
  public static class UnmarkedNested {
    public String get() { return ""; }
    public static String staticGet() { return ""; }

    @NullMarked
    public String markedGet() { return ""; }
  }
}
