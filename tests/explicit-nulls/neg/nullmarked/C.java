package unmarked;

import org.jspecify.annotations.*;

// Null-marked in a package that is not
@NullMarked
public class C {
  public String get() { return ""; }
  public static String staticGet() { return ""; }
  public static void staticSet(String s) {}
}
