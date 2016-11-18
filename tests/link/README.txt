Tests are in 3 folders dce, stdlib-dce and dce-failing.

dce: tests are compiled with -lik-dce (with and without -link-vis) without the standard library.

stdlib-dce: tests are compiled with -lik-dce (with and without -link-vis) with or without the standard library.

dce-failing: tests that fail in dce and stdlib-dce. If the test has 'stdlib' as a prefix then it should go into stdlib-dce

Note: tests in folder dce that have 'stdlib' as a prefix are tests that should go in stdlib-dce but are currently failing there.
