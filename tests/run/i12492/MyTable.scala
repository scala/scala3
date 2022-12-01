import scala.annotation.meta.field

type MyColumn = MyColumnBase @field

class MyTable(
  @(MyColumnBase @field)(name="BRAND_NAME")
  val brandName: String,
  @MyColumn(name="COMPANY_NAME")
  val companyName: String,
)
