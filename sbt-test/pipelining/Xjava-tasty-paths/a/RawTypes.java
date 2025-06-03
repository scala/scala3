package a;

public class RawTypes {

  public class C<T> {
    public class D<U> {
    }
  }

  public static void mii_Raw_Raw(RawTypes.C.D d) {
  }

  public static void mii_Raw_Raw2(C.D d) {
  }

}
