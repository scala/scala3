// Constants and nested types do not need a static initializer
public interface JavaSam<A> {
  int C = 1;
  String S = "s";
  interface Nested {}
  A apply(A x);
  default A twice(A x) { return apply(apply(x)); }
}
