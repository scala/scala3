package javax.annotation;
import java.util.*;

public class J {

    // --- @Nullable on result types ---

    // annotation at method level (common for JetBrains/JSR 305 style)
    @Nullable
    public String returnNullable() {
        return null;
    }

    // annotation inline before return type
    public static @Nullable String staticReturnNullable() {
        return null;
    }

    // annotation inline before generic return type
    public <T> @Nullable T genericReturnNullable(T a) {
        return null;
    }

    // annotation at method level, array return type
    @Nullable
    public String[] returnNullableArray() {
        return null;
    }

    // annotation inline before generic container return type
    public <T> @Nullable List<T> returnNullableGeneric(T a) {
        return null;
    }

    // --- @Nullable on argument types ---

    public String argNullable(@Nullable String s) {
        return s == null ? "null" : s;
    }

    public <T> String genericArgNullable(@Nullable T a) {
        return String.valueOf(a);
    }

    // --- @Nullable on type parameters ---

    public List<@Nullable String> returnListOfNullable() {
        List<String> xs = new ArrayList<>();
        xs.add(null);
        return xs;
    }

    public Map<String, @Nullable String> returnMapWithNullableValue() {
        Map<String, String> m = new HashMap<>();
        m.put("k", null);
        return m;
    }

    // --- @Nullable on fields ---

    // annotation at field level (common for JetBrains/JSR 305 style)
    @Nullable
    public String nullableField = null;

    @Nullable
    public static String staticNullableField = null;
}
