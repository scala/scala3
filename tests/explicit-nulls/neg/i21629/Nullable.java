package javax.annotation;

import java.lang.annotation.*;

// Mimics annotations from libraries like org.jetbrains and jspecify.
@Target({ElementType.TYPE_USE, ElementType.METHOD, ElementType.FIELD, ElementType.PARAMETER, ElementType.LOCAL_VARIABLE})
@Retention(RetentionPolicy.RUNTIME)
public @interface Nullable {
}
