public record R1(int i, String s) {
  public String getString() {
    return s + i;
  }

  public int getInt() {
    return 0;
  }
}
