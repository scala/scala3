package javax.annotation;
import java.util.*;

public class J_2 {

    // Since the value of the constant field is not null,
    // the type of the field is ConstantType("k"), which we
    // don't need to nullify
    public static final String k = "k";

    @Nonnull
    public static String l = "l";

    @Nonnull
    // Since the value of the constant field is null,
    // the type of the field before nullifying is TypeRef(String).
    // With the Nonnull annotation, the result of nullifying would
    // be TypeRef(String).
    public final String m = null;

    @Nonnull
    public String n = "n";

    @Nonnull
    public static final String f(int i) {
        return "f: " + i;
    }

    @Nonnull
    public static String g(int i) {
        return "g: " + i;
    }

    @Nonnull
    public String h(int i) {
        return "h: " + i;
    }

    @Nonnull
    public <T> String[] genericf(T a) {
        String[] as = new String[1];
        as[0] = "" + a;
        return as;
    }

    @Nonnull
    public <T> List<T> genericg(T a) {
        List<T> as = new ArrayList<T>();
        as.add(a);
        return as;
    }
}
