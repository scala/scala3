# #!/usr/bin/env bash
stable=$1

rc="$(($stable+1))"
next="$(($rc+1))"

stable_version="0.$stable.0"
rc_version="0.$rc.0-RC1"
next_version="0.$next.0"
stable_branch="0.$stable.x"
rc_branch="0.$rc.x"

LIST='- [ ] Publish artifacts to Maven via CI
  - [ ] On branch `<stable-branch>`, set `baseVersion` to `<stable-version>` and `git tag` it as `<stable-version>`. This will publish artefacts to Sonatype and GitHub Release
  - [ ] Merge branch `<stable-branch>` into `master` to guarantee that all of the `<stable-branch>` commits are propagated to `master`
  - [ ] Look at the milestone of the RC version being released. Move all the open issues from it to the next milestone.
  - [ ] Create branch `<rc-branch>` from `master`
  - [ ] On `<rc-branch>`, set `baseVersion` to `<rc-version>` and `git tag` it as `<rc-version>`. This will publish artefacts to Sonatype and GitHub Release.
  - [ ] On `master`, set `baseVersion` to `<next-version>`
- [ ] Update `scalaVersion` (and, if applicable, the `sbt-dotty` version) in the Dotty ecosystem projects
  - [ ] https://github.com/lampepfl/dotty-example-project
  - [ ] https://github.com/lampepfl/dotty-example-project/tree/mill
  - [ ] https://github.com/lampepfl/dotty.g8 [![Build Status](https://travis-ci.org/lampepfl/dotty.g8.svg?branch=master)](https://travis-ci.org/lampepfl/dotty.g8/)
    - [ ] Committed to `master`
  - [ ] https://github.com/lampepfl/dotty-cross.g8 [![Build Status](https://travis-ci.org/lampepfl/dotty-cross.g8.svg?branch=master)](https://travis-ci.org/lampepfl/dotty-cross.g8/)
    - [ ] Committed to `master`
  - [ ] https://github.com/lampepfl/homebrew-brew [![Build Status](https://travis-ci.org/lampepfl/homebrew-brew.svg?branch=master)](https://travis-ci.org/lampepfl/homebrew-brew)
    - [ ] Committed to `master`
    - SHA256 sum for the artifact: `wget -q -O- https://github.com/lampepfl/dotty/releases/download/<rc-version>/sha256sum.txt | grep ".tar.gz"`
  - [ ] https://github.com/lampepfl/packtest [![Build Status](https://travis-ci.org/lampepfl/packtest.svg?branch=master)](https://travis-ci.org/lampepfl/packtest)
    - [ ] Committed to `master`
  - [ ] https://github.com/scalacenter/scastie
    - [ ] PR submitted
    - [ ] PR merged
    - [ ] https://scastie.scala-lang.org/ -> Build Settings -> Dotty mentions `<rc-version>`
  - [ ] Dotty reference compiler [![Build Status](http://dotty-ci.epfl.ch/api/badges/lampepfl/dotty/status.svg)](http://dotty-ci.epfl.ch/lampepfl/dotty)
    - [ ] PR submitted
    - [ ] PR merged
  - [ ] Scalac [![Build Status](https://travis-ci.org/scala/scala.svg?branch=2.13.x)](https://travis-ci.org/scala/scala)
    - [ ] PR submitted
    - [ ] PR merged
- [ ] Announce the release
  - [ ] Publish releases for the RC and stable versions on GitHub Releases
  - [ ] Publish Blog Post on dotty.epfl.ch
  - [ ] Make an announcement thread on https://contributors.scala-lang.org
  - [ ] Tweet the announcement blog post on https://twitter.com/scala_lang

[Instructions on how to release](https://dotty.epfl.ch/docs/contributing/release.html)'

echo "$LIST" |\
  sed "s/<stable-version>/$stable_version/g" |\
  sed "s/<rc-version>/$rc_version/g" |\
  sed "s/<next-version>/$next_version/g" |\
  sed "s/<stable-branch>/$stable_branch/g" |\
  sed "s/<rc-branch>/$rc_branch/g"
