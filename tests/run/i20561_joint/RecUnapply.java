public record RecUnapply(int i, String s) {
  public static scala.Tuple2<Integer, String> unapply(RecUnapply r) {
    return new scala.Tuple2<>(1, "predefined");
  }
}
