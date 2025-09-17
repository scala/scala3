//> using options -language:experimental.erasedDefinitions

erased   enum Foo6 { case C } // error: only access modifiers allowed

enum Foo10 { // error: Enumerations must contain at least one case
  erased    case C6()  // error // error
}

enum Foo11 { // error: Enumerations must contain at least one case
  erased    case C6  // error // error
}
