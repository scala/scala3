package repeatable;

import java.lang.annotation.*;

@Repeatable(SecondLevel_0.class)
@Retention(RetentionPolicy.RUNTIME)
public @interface FirstLevel_0 {
  Plain_0[] value();
}