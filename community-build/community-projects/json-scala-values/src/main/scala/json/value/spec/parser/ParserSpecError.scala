package json.value.spec.parser

private[parser] object ParserSpecError {

  val SUCH_THAT_CONDITION_FAILED = "condition failed"
  val INVALID_JSON_TOKEN = "invalid Json token."
  val NULL_EXPECTED = "null expected"
  val START_ARRAY_EXPECTED = "start JSON array '[' expected"
  val START_OBJECT_EXPECTED = "start JSON object '{' expected"
  val KEY_CONDITION_FAILED = "key condition failed"
  val VALUE_CONDITION_FAILED = "value condition failed"
  val SPEC_FOR_VALUE_NOT_DEFINED = "strict parser is missing a spec"
  val KEY_REQUIRED = "key required"

}
