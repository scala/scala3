public class J {
  @SuppressWarnings(value = "a")
  public void f1() {}
  @SuppressWarnings("b")
  public void f2() {}
  @SuppressWarnings({"c", "d"})
  public void f3() {}
}

class JOtherTypes {
  @Annot(1)
  public void f1() {}
  @Annot(-2)
  public void f2() {}
  @Annot(m = false)
  public void f3() {}
  @Annot(m = true)
  public void f4() {}
  @Annot(n = 1.1)
  public void f5() {}
  @Annot(n = -2.1)
  public void f6() {}
}