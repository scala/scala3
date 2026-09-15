package org.jspecify.annotations;

import java.lang.annotation.*;

@Target({ElementType.TYPE_USE, ElementType.METHOD, ElementType.FIELD, ElementType.PARAMETER, ElementType.LOCAL_VARIABLE})
@Retention(RetentionPolicy.RUNTIME)
public @interface Nullable {
}
