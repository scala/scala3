// the vararg constructor is not the canonical one, the record is not vararg
public record RecVarNonCanon_1(Object o, String[] xs) {
  public RecVarNonCanon_1(Object o, int... rest) { this(o, new String[0]); }
}
