Tests are in 3 folders link-dce, link-dce-stdlib and link-dce-failing.

link-dce: tests are compiled with -lik-dce (with and without -link-vis) without the standard library.

link-dce-stdlib: tests are compiled with -lik-dce (with and without -link-vis) with or without the standard library. It is important
            to only use directories for tests as this configuration is required to add the stdlib files correctly.

link-dce-failing: tests that fail in dce and link-dce-stdlib. If the test has 'stdlib' as a prefix then it should go into link-dce-stdlib

Note: tests in folder dce that have 'stdlib' as a prefix are tests that should go in link-dce-stdlib but are currently failing there.
