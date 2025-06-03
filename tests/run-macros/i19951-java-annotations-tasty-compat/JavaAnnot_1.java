
import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@interface JavaAnnot {
  int a();
  String b() default "empty";
  int c() default 5;
}
