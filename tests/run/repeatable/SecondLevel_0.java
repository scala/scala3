package repeatable;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface SecondLevel_0 {
  FirstLevel_0[] value();
}
