package org.jspecify.annotations;

import java.lang.annotation.*;

// Minimal stub of JSpecify's scope marker. Package-private so it can live in a `_1`-suffixed
// file (javac only enforces the public-class/filename match for public types).
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.PACKAGE})
@Retention(RetentionPolicy.RUNTIME)
@interface NullMarked {
}
