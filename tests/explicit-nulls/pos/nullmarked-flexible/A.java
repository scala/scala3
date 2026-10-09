package a;

import org.jspecify.annotations.*;

@NullMarked
public class A {
  public String get() { return ""; }
  public @Nullable String getNullable() { return null; }
  public void varargs(String... xs) {}
  public void nullableElemVarargs(@Nullable String... xs) {}
  public void nullableVarargs(String @Nullable ... xs) {}
}
