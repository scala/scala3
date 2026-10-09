package org.jspecify.annotations;

import java.lang.annotation.*;

// Stubs of the JSpecify annotations, with the same targets as the real ones

@Target({ElementType.MODULE, ElementType.PACKAGE, ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface NullMarked {}
