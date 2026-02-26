package i19643;

@interface MyAnnot {
  Class<?> reify() default Object.class;
  Class<?>[] args() default {};
}

class MyAnnotated {

  @MyAnnot(reify = String.class)
  public static int method1() { return 23; }

  @MyAnnot(reify = int.class)
  public static int method2() { return 23; }

  @MyAnnot(reify = long.class)
  public static int method3() { return 23; }

  @MyAnnot(args = {String.class})
  public static int method4() { return 23; }

  @MyAnnot(args = {int.class})
  public static int method5() { return 23; }

  @MyAnnot(args = {long.class})
  public static int method6() { return 23; }

}
