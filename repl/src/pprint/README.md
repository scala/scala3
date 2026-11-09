Vendored from lihaoyi/pprint commit 66733cc563c1a4c8d694b3daba69ab643a50dd79

Changes:
- Added `package dotty.vendored`
- `[_]` -> `[?]`
- `private[this]` -> `private`
- `:_*` -> `*`
- added `using` where necessary for implicit parameters
- added an unsafe cast for `null` since the original builds without explicit nulls
- added a cast to `String | Null` for `toString` since the original builds without explicit nulls
- Commented out an unreachable match case in `TPrintImpl` (line 116)
- Added a REPL hook to honor user-defined `Product.toString` implementations
