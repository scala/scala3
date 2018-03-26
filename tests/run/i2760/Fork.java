import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface Fork {
    int value() default -1;
    int warmups() default -1;
}
