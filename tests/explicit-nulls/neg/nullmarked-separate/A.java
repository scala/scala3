package unmarked;

import org.jspecify.annotations.*;

@NullMarked
public class A {
  public @Nullable String[] nullableElements() { return null; }
  public String @Nullable [] nullableArray() { return null; }
  public void varargs(@Nullable String... xs) {}
  public void nonNullVarargs(String... xs) {}
}
