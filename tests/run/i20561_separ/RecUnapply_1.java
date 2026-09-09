public record RecUnapply_1(int i, String s) {
  public static scala.Tuple2<Integer, String> unapply(RecUnapply_1 r) {
    return new scala.Tuple2<>(1, "predefined");
  }
}
