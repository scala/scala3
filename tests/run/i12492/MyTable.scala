import scala.annotation.meta.*

type FieldColumn = MyColumnBase @field
type GetterColumn = MyColumnBase @getter

class MyTable(
  @(MyColumnBase @field)(name="FIELD_NAME1")
  val fieldName1: String,
  @FieldColumn(name="FIELD_NAME2")
  val fieldName2: String,

  @(MyColumnBase @getter)(name="GETTER_NAME1")
  val getterName1: String,
  @GetterColumn(name="GETTER_NAME2")
  val getterName2: String,
)
