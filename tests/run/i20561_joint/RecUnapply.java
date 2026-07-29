public record RecUnapply(int i, String s) {
  public static RecUnapply unapply(RecUnapply r) { return new RecUnapply(1, "predefined"); }
}
