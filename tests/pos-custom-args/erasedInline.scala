import language.experimental.erasedDefinitions

erased inline def f: Unit = () // error: illegal combination of modifiers: `erased` and `inline` for: method f
inline def g: Unit = ()
