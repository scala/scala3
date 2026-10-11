## Specialized Trait Benchmarks

### Run
1. Publish the compiler first and check that the version in the benchmark file corresponds to the generated version,
2. Kill bloop server if republishing an existing version:
  `scala-cli --power bloop exit`
3. Run the benchmark with:
  ```
scala-cli --power --jmh bench-micro/src/main/scala/dotty/tools/benchmarks/specializedtraits/VectorDotProduct.scala
  ```

### Troubleshooting
- May have to run it again / delete .scala-build and rerun if you get a class not found error from `scala-cli` first time - the `--jmh` flag is still experimental.
