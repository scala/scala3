package marked;

import org.jspecify.annotations.*;

@NullUnmarked
public class U {
  public String get() { return ""; }
  public static String staticGet() { return ""; }

  @NullMarked
  public String markedGet() { return ""; }

  // Both annotations behave like neither: unmarked like the class
  @NullMarked @NullUnmarked
  public String bothGet() { return ""; }
}
