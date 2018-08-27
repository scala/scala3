enum Foo { case A }
enum Bar { case A }
enum Baz extends Foo { case Z } // error

enum Quux extends Foo with Bar { case Z } // error
