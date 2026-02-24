package javax.annotation;
import java.util.*;

public class J {

    // @Nullable on argument type
    public String argNullable(@Nullable String s) {
        return s == null ? "null" : s;
    }

    // @Nullable on result type
    public @Nullable String returnNullable() {
        return null;
    }

    // @Nullable on type parameter
    public List<@Nullable String> returnListOfNullable() {
        return new ArrayList<>();
    }
}
