package a;

// a record with an explicitly declared canonical constructor,
// and a secondary constructor declared before it
public record RecExpl(int x, String y) {
  public RecExpl(int x) { this(x, "default"); }
  public RecExpl(int x, String y) { this.x = x; this.y = y; }
}
