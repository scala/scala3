enum A extends AnyRef { } // error: missing case

enum B { def foo = 1 } // error: missing case

enum C // error: missing case
// error: '{' expected, but eof found