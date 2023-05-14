# Scala 3 Community Build

This project contains tests to build and test a corpus of open sources Scala
projects against the latest version of Scala 3.

## Running it locally

To run the community build on a local machine, first fetch all the git
submodules with `git submodule update --init` and run `sbt community-build/test`
from the root of the dotty repo.

To run a single project, you can use the usual syntax for running a single JUnit
test, for example `community-build/testOnly -- *shapeless`

In CI the community build is split up into 3 seperate groups: A, B, and C. To
run one specific build you can also use the same JUnit syntax as above targeting
the individual group. For example:

```
sbt "community-build/testOnly dotty.communitybuild.CommunityBuildTestA"
```

## Adding your project

The community build is able to handle both Mill and sbt projects. To add your
project to the community build you can follow these steps:

1. Ensure your project is compiling with Scala 3. If you need help make sure to
   check out the [Scala 3 Migration
   Guide](https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html).
   You can see the submodules in
   [community-projects](https://github.com/lampepfl/dotty/tree/main/community-build/community-projects/)
   for examples of projects that compile with Scala 3.

2. Open a PR against this repo that:
     - Adds your project as a new git submodule
       - `git submodule add https://github.com/dotty-staging/XYZ.git community-build/community-projects/XYZ`
     - Add the project to [projects.scala](https://github.com/lampepfl/dotty/blob/main/community-build/src/scala/dotty/communitybuild/projects.scala)
     - Adds a test in [CommunityBuildTest.scala](https://github.com/lampepfl/dotty/blob/main/community-build/test/scala/dotty/communitybuild/CommunityBuildTest.scala)

3. Once the CI is green, someone from the Dotty team will fork your repo and add
   it to [dotty-staging](https://github.com/dotty-staging). This enables us to
   make changes to your fork if necessary to keep the community build running
   smoothly.

4. Once the fork is created, please update your PR to point to this new fork
   instead of your repo.

## Updating a project

The projects included in the community build are all forked and located in
[dotty-staging](https://github.com/dotty-staging). When something needs to be
bumped the process is as follows:

1. Fork the dotty staging repo and sync it with the upstream project.

2. Once you've verified that the tests are all passing you can then either
   request in your PR that the dotty-staging fork be synced or in the
   [scala-contributors](https://discord.com/channels/632150470000902164/632628489719382036)
   discord channel.

### Some helpful tips

- If you're unfamiliar with Git Submodules you can find a nice guide to get
    familiar with them [here](https://git-scm.com/book/en/v2/Git-Tools-Submodules).
- Keep in mind that many projects are interrelated. So when you bump one that
    change may cascade through multiple different projects causing you to have
    to bump multiple. Plan accordingly and at times it's best to pin it to a
    stable release version, especially if it's a root library that many others
    in the community build are relying on.

## Looking for the "unmanaged" Scala 3 community build?

You can find this [here](https://github.com/VirtusLab/community-build3).

