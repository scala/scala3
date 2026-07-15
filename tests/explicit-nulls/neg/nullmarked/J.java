package nullmarked;

import org.jspecify.annotations.*;

// The whole class is `@NullMarked`, so unannotated reference types are non-null.
@NullMarked
public class J {

    // Unannotated -> non-null String.
    public String get() {
        return "";
    }

    // Type-use `@Nullable` still reintroduces nullability -> String | Null.
    public @Nullable String getNullable() {
        return null;
    }

    // Unannotated parameter -> non-null String.
    public void set(String s) {
    }

    // `@NullUnmarked` restores the implicit-nulls default within this method,
    // so the parameter is nullable again.
    @NullUnmarked
    public void unmarkedSet(String s) {
    }
}
