package org.jspecify.annotations;

import java.lang.annotation.*;

// Minimal stub of JSpecify's scope marker.
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.PACKAGE})
@Retention(RetentionPolicy.RUNTIME)
public @interface NullMarked {
}
