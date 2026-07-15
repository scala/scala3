package nmpkg;

import org.jspecify.annotations.*;

// No class-level annotation: nullability is decided by the package's `@NullMarked` (package-info).
public class J {

    // Unannotated -> non-null String (package is null-marked).
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

    // `@NullUnmarked` overrides the package marking for this method.
    @NullUnmarked
    public void unmarkedSet(String s) {
    }
}
