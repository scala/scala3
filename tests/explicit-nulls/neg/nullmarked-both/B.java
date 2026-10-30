package both;

public class B {
  public String get() { return ""; }

  @org.jspecify.annotations.NullMarked
  public String markedGet() { return ""; }
}
