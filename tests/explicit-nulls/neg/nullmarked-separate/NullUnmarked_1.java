package org.jspecify.annotations;

import java.lang.annotation.*;

// Minimal stub of JSpecify's scope marker that re-enables implicit nullability.
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.PACKAGE})
@Retention(RetentionPolicy.RUNTIME)
@interface NullUnmarked {
}
