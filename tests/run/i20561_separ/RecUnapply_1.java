public record RecUnapply_1(int i, String s) {
  public static RecUnapply_1 unapply(RecUnapply_1 r) { return new RecUnapply_1(1, "predefined"); }
}
