import java.lang.annotation.*;

@Repeatable(Annot1.Container.class)
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@interface Annot1 { String value() default "";

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE)
    public static @interface Container {
        Annot1[] value();
    }
}
