package nmpkg;

import org.jspecify.annotations.*;

// `@NullUnmarked` on a top-level class overrides the package's `@NullMarked`,
// restoring the implicit-nulls default for its members.
@NullUnmarked
public class U {

    // Unmarked -> String | Null.
    public String get() {
        return "";
    }

    // Unmarked parameter -> nullable.
    public void set(String s) {
    }

    // `@NullMarked` re-marks this method, so its result is non-null again.
    @NullMarked
    public String markedGet() {
        return "";
    }

    // Both markers behave as if neither is present, so this inherits `U`'s `@NullUnmarked`
    // (its result is `String | Null`), rather than letting `@NullMarked` win.
    @NullMarked
    @NullUnmarked
    public String bothGet() {
        return "";
    }
}
