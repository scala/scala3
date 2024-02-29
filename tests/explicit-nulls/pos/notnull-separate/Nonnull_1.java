package javax.annotation;

import java.lang.annotation.*;

// A "fake" Nonnull Annotation for jsr305
@Retention(value = RetentionPolicy.RUNTIME)
@interface Nonnull {
}
