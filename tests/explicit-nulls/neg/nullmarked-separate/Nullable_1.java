package org.jspecify.annotations;

import java.lang.annotation.*;

// Minimal stub of JSpecify's type-use nullable marker.
@Target({ElementType.TYPE_USE, ElementType.METHOD, ElementType.FIELD, ElementType.PARAMETER, ElementType.LOCAL_VARIABLE})
@Retention(RetentionPolicy.RUNTIME)
@interface Nullable {
}
