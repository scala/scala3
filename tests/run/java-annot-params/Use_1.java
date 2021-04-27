package annots;

@WithClass(arg = A.class)
@WithClassDefaultName(A.class)
@WithString(arg = "SOME STRING")
@WithReference(arg = A.CONST)
@WithBoolean(arg = false)
@WithFloat(arg = 13.7f)
@WithNested(arg = @Nested("VALUE"))
@WithArray({"a", "b", "c"})
@WithEmptyArray({})
@WithSingleElement("SINGLE")
@WithMultipleArgs(
  ints = { 1, 2, 3, },
  annots = { @Nested("Value"),
  @Nested(A.CONST) },
  floatVal = 13.7f,
  value = "ABC",
  clazz = A.class
)
public class Use_1 {}