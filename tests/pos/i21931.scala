object Test {
  def f() = {
    val NotFound: Char = 'a'
    class crashing() {
      class issue() {
        NotFound
      }
      class Module() {
        val obligatory = {
          def anonIssue = {
            issue()
          }
          anonIssue
        }
      }
    }
  }
}
