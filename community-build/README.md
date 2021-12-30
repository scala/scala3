# Scala 3 Community Build

This project contains tests to build and test a corpus of open sources Scala projects against the latest version of Scala 3.

## Running it locally

To run the community build on a local machine, first fetch all the git submodules with `git submodule update --init` and run `sbt community-build/test` from the root of the dotty repo.

To run a single project, you can use the usual syntax for running a single JUnit test, for example `community-build/testOnly -- *shapeless`

## Adding your project

To add your project to the community build you can follow these steps:

1. Get your project to compile with Scala 3. Instructions can be found on the [scala3-example-project](https://github.com/lampepfl/scala3-example-project).
   See the submodules in [community-projects](https://github.com/lampepfl/dotty/tree/master/community-build/community-projects/) for examples of projects that compile with Scala 3.

2. Open a PR against this repo that:
     - Adds your project as a new git submodule
       - `git submodule add https://github.com/lampepfl/XYZ.git community-build/community-projects/XYZ`
     - Add the project to [projects.scala](https://github.com/lampepfl/dotty/blob/master/community-build/src/scala/dotty/communitybuild/projects.scala).
     - Adds a test in [CommunityBuildTest.scala](https://github.com/lampepfl/dotty/blob/master/community-build/test/scala/dotty/communitybuild/CommunityBuildTest.scala).
     - Make sure the test supports `-Ysafe-init`.
