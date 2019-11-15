## BlackBox Todo list
This todolist is based upon blackbox observation of testcases at this repository: https://github.com/scalameta/scalameta/tree/master/tests/jvm/src/test/resources/example

- [ ] Hide unapply, unapplySeq?
- [ ] Hide local vars in pattern def?
- [ ] Hide desugaring of for comprehension
- [ ] Hide calls to implicit conversions and their arguments
- [ ] Hide Tuple contructors
- [ ] Hide implicitly applied arguments
- [x] Assignments to vars should emit uses of the setter method and not the getter
- [x] Hide constructor of module template // expect tests hide primary ctors
- [x] unescape unicode in names.
- [x] Only traverse prefix of a Select if it is not a package
- [x] Ensure only methods are counted in overloads
- [x] Import wildcard needs to have reference to the qualifier
- [-] ~~Hide `scala.Predef` prefix~~ // same effect acheved by ensuring prefix has a range > 0
- [-] ~~Hide `scala.StringContext`~~ // same effect acheved by ensuring prefix has a range > 0
- [-] ~~Hide `Predef.classOf`~~ // won't fix - confirmed as a bug in scalameta
