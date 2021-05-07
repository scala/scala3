// This is necessary because tests are run in batch mode
object CompileState {
  @volatile var isNew: Boolean = false
}
