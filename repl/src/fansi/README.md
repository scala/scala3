Vendored from lihaoyi/fansi commit c8d4e5b56c46f2b360a441ca6d35620819a1ee52

Changes:
- `private[this]` -> `private`
- `protected[this]` -> `protected`
- `:_*` -> `*`
- added `using` where necessary for implicit parameters
- added an unsafe cast for `null` since the original builds without explicit nulls
