package scala.compiletime

package object testing {

  /** Whether the code type checks in the current context?
   *
   *  @param code The code to be type checked
   *
   *  @return false if the code has syntax error or type error in the current context, otherwise returns true.
   *
   *  The code should be a sequence of expressions or statements that may appear in a block.
   */
  inline def typeChecks(inline code: String): Boolean =
    error("`typeChecks` was not checked by the compiler")

  /** Whether the code type checks in the current context? If not,
   *  returns a list of errors encountered on compilation.
   *  IMPORTANT: No stability guarantees are provided on the format of these
   *  errors. This means the format and the API may change from
   *  version to version. This API is to be used for testing purposes
   *  only.
   *
   *  @param code The code to be type checked
   *
   *  @return a list of errors encountered during parsing and typechecking.
   *
   *  The code should be a sequence of expressions or statements that may appear in a block.
   */
  inline def typeCheckErrors(inline code: String): List[Error] =
    error("`typeCheckErrors` was not checked by the compiler")

}
