import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@InterfaceAudience_JAVA_ONLY_1.Public(bytes="yes")
public class InterfaceStability_JAVA_ONLY_1 {
    @Retention(RetentionPolicy.RUNTIME)
    public @interface Evolving { String bytes(); }
}
