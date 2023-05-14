class SCALA_ONLY_Invalid2 {

    // Closing delimiter is first three eligible `"""`, not last
    public static final String closingDelimiterIsNotScalas = """
      foo"""";  // error
} // anypos-error
