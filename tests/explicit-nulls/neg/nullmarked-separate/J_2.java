package org.jspecify.annotations;

// Lives in the same package as the (package-private) annotation stubs so it can use them.
// This class is compiled to a classfile first and read back by ClassfileParser.
// The whole class is `@NullMarked`, so unannotated reference types are non-null.
@NullMarked
public class J_2 {

    // `@NullUnmarked` on a constructor makes its parameters nullable again.
    @NullUnmarked
    public J_2(String s) {
    }

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

    // Annotated with both markers -> behaves as if neither is present, so this method inherits
    // the class's `@NullMarked` and its parameter stays non-null.
    @NullMarked
    @NullUnmarked
    public void bothSet(String s) {
    }

    // `@NullUnmarked` on a nested class restores the implicit-nulls default for its members.
    @NullUnmarked
    public static class Inner {

        // Unmarked -> String | Null.
        public String innerGet() {
            return "";
        }

        // Unmarked parameter -> nullable.
        public void innerSet(String s) {
        }

        // `@NullMarked` re-marks within the unmarked nested class (nearest scope wins),
        // so this result is non-null again.
        @NullMarked
        public String remarkedGet() {
            return "";
        }
    }
}
