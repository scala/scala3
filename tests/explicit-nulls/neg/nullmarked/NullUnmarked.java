package org.jspecify.annotations;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD, ElementType.MODULE, ElementType.PACKAGE, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface NullUnmarked {

}
