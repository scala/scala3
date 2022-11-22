// note that scala's java parser doesn't care about the platform version
class SCALA_ONLY_Invalid1 {

    public static final String badOpeningDelimiter = """non-whitespace  // error
      foo
    """;  // error
}
