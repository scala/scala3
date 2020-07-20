public class J_2 extends D<String> { // error

  public J_2() {
    super(null);
  }

  public static D<String> getDS() { // error
    return new D<String>("DS");
  }

  public static final D<String> fieldDS = new D<String>("DS"); // error

  public static void useDS(D<String> ds) {} // error

  public static <A extends D<String>> void genericDS() {} // error

  public static void useOK(D<?> ds) {}

  public static D<?> getOK() {return null;}

  public static <A extends D<?>> D<?> genericOK(A a) {return a;}

}
