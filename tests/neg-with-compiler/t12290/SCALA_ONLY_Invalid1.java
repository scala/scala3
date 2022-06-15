// test: -jvm 15+
class SCALA_ONLY_Invalid1 {

    public static final String badOpeningDelimiter = """non-whitespace  // error
      foo
    """;  // error
}
