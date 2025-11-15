public class JavaPart {
  public interface A { }
  public interface B extends A {
      int onlyInB();
  }

  public interface Lvl1 {
    A[] getData();
  }

  public interface Lvl2 extends Lvl1 {
    @Override
    B[] getData();
  }

  public interface Lvl3 extends Lvl2, Lvl1 { }
}
