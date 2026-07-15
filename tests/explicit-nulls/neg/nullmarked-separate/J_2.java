package org.jspecify.annotations;

// Lives in the same package as the (package-private) annotation stubs so it can use them.
// This class is compiled to a classfile first and read back by ClassfileParser.
// The whole class is `@NullMarked`, so unannotated reference types are non-null.
@NullMarked
public class J_2 {

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
