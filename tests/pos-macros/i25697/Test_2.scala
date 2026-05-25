import java.util.regex.Pattern

@main def Test(): Unit =
  inline def regex   = "SomeString"
  inline def pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE | Pattern.COMMENTS)

  TinyMacro.inspect(pattern.matcher("value").matches())
