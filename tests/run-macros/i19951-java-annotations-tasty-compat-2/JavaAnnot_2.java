
import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@interface JavaAnnot {
  int c() default 5;
  int a();
  int d() default 42;
  String b() default "empty";
}
