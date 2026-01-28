package javax.annotation;

import java.lang.annotation.*;

// A "fake" Nullable Annotation for jsr305
@Retention(value = RetentionPolicy.RUNTIME)
@interface Nullable {
}
