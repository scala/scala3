package javax.annotation;
import java.util.*;

public class J {

    @Nonnull
    // JavaParser will never assign ConstantType to fields currently.
    public static final String k = "k";

    @Nonnull
    public static String l = "l";

    @Nonnull
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
