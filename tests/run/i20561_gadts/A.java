public sealed interface A<X> {
  public X x();

  public static record A1(Integer x) implements A<Integer>{}
  public static record A2(Boolean x) implements A<Boolean>{}
}
