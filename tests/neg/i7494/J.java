public class J {
  // for java, D is D<T extends Object>
  public static D<String> getDS() { // error
    return new D<String>(DS);
  }
}

