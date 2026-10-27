package org.jspecify.annotations;

import java.lang.annotation.*;

@Target({ElementType.PACKAGE, ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface NullUnmarked {}
