import suggestions.SearchRoot

// nopos-error
// nopos-error
val missing = 0.noSuchMember // error

// where
// ```java
// package suggestions;
// public class SearchRoot extends broken.Child {}
// ```
// broken.Child extends Outer.Inner.
// While `Outer$Inner.class` exists, there's missing `Outer.class`.
