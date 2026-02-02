//> using options -Werror -Wunused:all -Xcheck-macros
@main def Test =
  TestBuilder.test:
    val start @ _: String = "" // Converting this to a match expression resolves the error
    // Alternative: val Seq(start) = Seq("")
    print(start)
