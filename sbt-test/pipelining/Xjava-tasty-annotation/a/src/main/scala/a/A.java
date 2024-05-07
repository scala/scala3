// this test ensures that it is possible to read a java annotation from TASTy.
package a;

import java.lang.annotation.*;


@Documented
@Retention(RetentionPolicy.CLASS)
@Target({ ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.FIELD, ElementType.TYPE, ElementType.PACKAGE })
public @interface A {
}
