// Sample main method
object myProgram:

  /** Exits program with error code */
  @main def exit(code: Int): main.ExitCode =
    println(f"Exiting with code $code")
    main.ExitCode(code)

end myProgram
