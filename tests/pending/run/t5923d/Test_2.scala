class RowA extends MappedRow
class RowB extends MappedRow

object Test extends dotty.runtime.LegacyApp {
  implicitly[RowMapper[RowA]]
  implicitly[RowMapper[RowB]]
}