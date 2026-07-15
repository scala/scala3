// The whole package is `@NullMarked` via `package-info.java`, so unannotated reference types
// in this package are non-null. JavaParsers must read this package-level annotation.
@NullMarked
package nmpkg;

import org.jspecify.annotations.*;