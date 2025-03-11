import java.lang.annotation.*;
public @Retention(RetentionPolicy.RUNTIME)
@interface JavaAnnot {
  public String value();
}