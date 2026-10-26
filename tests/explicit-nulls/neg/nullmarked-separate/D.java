package unmarked;

public class D {
  public String get() { return ""; }

  @org.jspecify.annotations.NullMarked
  public String markedGet() { return ""; }

  @org.jspecify.annotations.NullMarked
  public D(String s) {}
}
