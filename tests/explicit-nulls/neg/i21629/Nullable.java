package javax.annotation;

import java.lang.annotation.*;

// A "fake" Nullable Annotation for jsr305
@Retention(value = RetentionPolicy.RUNTIME)
@Target(value = ElementType.TYPE_USE)
@interface Nullable {
}
