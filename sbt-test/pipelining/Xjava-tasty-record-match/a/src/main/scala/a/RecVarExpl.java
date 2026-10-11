package a;

// a vararg record with a secondary constructor, so the canonical (vararg)
// constructor must be picked out among several when loaded from a class file
public record RecVarExpl(int x, String... xs) {
  public RecVarExpl(int x) { this(x, new String[0]); }
}
