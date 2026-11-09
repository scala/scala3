package pkg;

public class Facade extends Impl {
  private Facade() {}
  public static String hidden() { return "Facade.hidden"; }
  public static String over(String x) { return "Facade.over(String)"; }
}
