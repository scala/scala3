import scala.annotation.meta.{ field as fld, getter as get, param as par }

type FldColumn = MyColumnBase @fld
type GetColumn = MyColumnBase @get
type ParColumn = MyColumnBase @par

class MyTable(
  @(MyColumnBase     ) val aaaParam1: String,
  @(MyColumnBase @fld) val fldParam1: String,
  @(MyColumnBase @get) val getParam1: String,
  @(MyColumnBase @par) val parParam1: String,
) {
  @(MyColumnBase     ) val aaaField1: String = ""
  @(MyColumnBase @fld) val fldField1: String = ""
  @(MyColumnBase @get) val getField1: String = ""
  @(MyColumnBase @par) val parField1: String = ""
}

class MyTable2(
  @FldColumn val fldParam2: String,
  @GetColumn val getParam2: String,
  @ParColumn val parParam2: String,
) {
  @FldColumn val fldField2: String = ""
  @GetColumn val getField2: String = ""
  @ParColumn val parField2: String = ""
}
