package example;

import java.lang.annotation.Annotation;

public class NamedImpl implements Named {
  public Class<? extends Annotation> annotationType() {
    return Named.class;
  }

  public String toString() { return "NamedImpl"; }
}
