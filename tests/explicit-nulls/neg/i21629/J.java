package javax.annotation;
import java.util.*;

public class J {

    private static String getK() {
        return "k";
    }

    @Nullable
    public static final String k = getK();

    @Nullable
    public static String l = "l";

    @Nullable
    public final String m = null;

    @Nullable
    public String n = "n";

    @Nullable
    public static final String f(int i) {
        return "f: " + i;
    }

    @Nullable
    public static String g(int i) {
        return "g: " + i;
    }

    @Nullable
    public String h(int i) {
        return "h: " + i;
    }

    @Nullable
    public String q(String s) {
        return "h: " + s;
    }

    @Nullable
    public <T> String[] genericf(T a) {
        String[] as = new String[1];
        as[0] = "" + a;
        return as;
    }

    @Nullable
    public <T> List<T> genericg(T a) {
        List<T> as = new ArrayList<T>();
        as.add(a);
        return as;
    }

    public List<@Nullable String> listS(String s) {
        List<String> as = new ArrayList<String>();
        as.add(null);
        return as;
    }
}
