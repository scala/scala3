import java.util.concurrent.TimeUnit;

public @interface MyAnnotation {
  public TimeUnit D() default TimeUnit.DAYS;
  TimeUnit C() default TimeUnit.DAYS;
  String A() default "";
  public String B() default "";
}