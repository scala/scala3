---
layout: doc-page
title: Release Procedure
---

# Model
The easiest way to produce a release of a GitHub-based open-source software is to tag the most recent commit on the `master` with the version number at regular intervals of time or once a previously agreed milestone is reached. However, this approach to releasing would rest on the assumption that each commit at the `master` branch can potentially be made into a release. We cannot provide the release-grade quality guarantees for each of the `master` commits, though.

Consequently, in Dotty, we are using the method above – releasing-by-tagging – to mark release candidates (RC’s) and not the stable releases. The stable releases are also marked by a tag, but we have a procedure to assure their quality.

An RC is promoted to a stable release in one release cycle after its creation. The idea is that this way, we have one release cycle's time to examine the release candidate and find critical issues which cannot be allowed into a stable release.

If such issues are found, their fixes end up on a separate branch dedicated to that release. In one release cycle after the RC creation, the RC, along with all its subsequent fixes, is promoted to a stable release by means of tagging it.

# Example
Say we want to release the 0.14.0 version. In this section we describe the process to do so (at a glance).

## At the Dotty Repo
1. Tag the latest `master` commit as `0.14.0-RC1`. This commit is the release candidate for the `0.14.0` version.
2. Create a branch from that commit called `0.14.x`. This branch is intended to host the subsequent fixes to the RC for the issues that cannot be allowed in the `0.14.0` stable release.
3. Up until the next release date, if we find some issues with `0.14.0-RC1` that cannot end up in the release, we push the fixes to the `0.14.x` branch.
4. At the next release date, we release `0.14.0` from the branch `0.14.x`. We do so by tagging the latest commit at the `0.14.x` branch as `0.14.0`. Some things to note here:
  1. At this point, `0.14.x` (the branch) and `0.14.0-RC1` (the tag at which `0.14.x` branched from `master`) may differ, and the `0.14.x` branch is a more mature version of the `0.14.0-RC1` tag.
  2. `0.14.0` is not the same as the `master`. Only the commits critical for the `0.14.0` release end up at `0.14.x` branch. Not all of the commits made to the `master` during the release cycle are critical to `0.14.0`. However, all of the commits from `0.14.x` must end up on the `master` branch, so we merge `0.14.x` into `master`.
5. After the `0.14.0` version is released, we start the process for releasing `0.15.0` – repeat this algorithm from the beginning with the version set to `0.15.0-RC1` at step (1).

## At the CI
CI is set to automatically detect the tags of the format discussed above and perform the required release operations. Precisely, it will do two things for the release tags:

- Publish the release jars to Maven
- Create the drafts at the GitHub [release](https://github.com/lampepfl/dotty/releases) page of the repository with the artefacts of the release.

The CI operation is entirely automatic provided you have tagged the release correctly. No need to do anything here.

## Documentation
### Release Procedure Checklist
Before we start the release procedure, we create an issue with a release checklist. As we go through the release, we update the checklist. In the checklist template below, the following variables are used:

- `<stable-version>` is the stable version being released, e.g. `0.14.0`. It is identical to the git tag under which it is released.
- `<rc-version>` is the version of the RC version being released, e.g. `0.15.0-RC1`. It is identical to the git tag under which it is released.
- `<stable-branch>` is the name of the stable branch being released, e.g. `0.14.x` if we are releasing `0.14.0`
- `<rc-branch>` is the RC's branch. E.g. if we are releasing `0.14.0` and `0.15.0-RC1`, the RC version is `0.15.0-RC1`, and `<rc-branch>` is `0.15.x`.
- `<next-version>` is the tech preview version. It is the next version after the currently released RC. E.g. if we are releasing `0.15.0-RC1`, this variable will be `0.16.0`.

A good workflow is to compute these variables before each release, then replace them in the checklist below:

```
- [ ] Publish artifacts to Maven via CI
  - [ ] On branch`<stable-branch>`, set `baseVersion` to `<stable-version>` and `git tag` it as `<stable-version>`. This will publish artefacts to Sonatype and GitHub Release
  - [ ] Merge branch `<stable-branch>` into `master` to guarantee that all of the `<stable-branch>` commits are propagated to `master`
  - [ ] Look at the milestone of the RC version being released. Move all the open issues from it to the next milestone.
  - [ ] Create branch `<rc-branch>` from `master`
  - [ ] On `<rc-branch>`, set `baseVersion` to `<rc-version>` and `git tag` it as `<rc-version>`. This will publish artefacts to Sonatype and GitHub Release.
  - [ ] On `master`, set `baseVersion` to `<next-version>`
- [ ] Update `scalaVersion` (and, if applicable, the `sbt-dotty` version) in the Dotty ecosystem projects
  - [ ] https://github.com/lampepfl/dotty-example-project 
  - [ ] https://github.com/lampepfl/dotty-example-project/tree/mill
  - [ ] https://github.com/lampepfl/dotty.g8
  - [ ] https://github.com/lampepfl/dotty-cross.g8
  - [ ] https://github.com/lampepfl/homebrew-brew
  - [ ] https://github.com/lampepfl/packtest
  - [ ] https://github.com/scalacenter/scastie
  - [ ] Dotty itself – update the `referenceVersion` in the `master` branch to `<rc-version>`. This is the version of Dotty used to compile the Dotty codebase on `master`.
  - [ ] Scalac CI, like this: scala/scala#7993
- [ ] Announce the release
  - [ ] Publish releases for the RC and stable versions on GitHub Releases
  - [ ] Publish Blog Post on dotty.epfl.ch
  - [ ] Make an announcement thread on https://contributors.scala-lang.org
  - [ ] Tweet the announcement blog post on https://twitter.com/scala_lang
```

### GitHub Releases and Blog Post
After the release is done, we document it as follows:

- On the GitHub release page, modify the release drafts created by CI. The RC draft should include notable changes introduced since the previous RC. E.g. for `0.14.0-RC1` these are generated by `gren changelog -G --override -D prs --tags=0.13.0-RC1..0.14.0-RC1`. `gren` is available [here](https://github.com/github-tools/github-release-notes), and before running the above command, please make sure that (1) the `origin` branch points to the `lampepfl/dotty` repository and (2) the two tags mentioned in the command are pushed to the `master` branch of that repo. Otherwise, the command won't pick up the tags.
- Create a blog article documenting the most important changes done by the release.

## Ecosystem
After releasing a new version of Dotty, we need to make sure to update the following related projects:

- [Example Project](https://github.com/lampepfl/dotty-example-project)
- [Example Project with Mill](https://github.com/lampepfl/dotty-example-project/tree/mill)
- [Dotty G8 template](https://github.com/lampepfl/dotty.g8)
- [Dotty G8 template with cross build support](https://github.com/lampepfl/dotty-cross.g8)
- [Dotty Homebrew Formula](https://github.com/lampepfl/homebrew-brew)
- [Dotty test under various OSs](https://github.com/lampepfl/packtest)
- 🚫[Scastie](https://github.com/scalacenter/scastie/) – FTTB doesn't work with the new Dotty releases because Scastie uses sbt 0.13.

For each need to do the following:

- Update Dotty version to the latest RC
- Update the sbt-dotty SBT plugin version to the latest published one
- Update the projects' source code to follow the Dotty developments if necessary

# Procedure in Bash Scripts
The below procedure is compiled from [this](https://github.com/lampepfl/dotty/issues/5907#issue-409313505) and [this](https://github.com/lampepfl/dotty/issues/6235#issue-429265748) checklists. It assumes we want to publish the `0.14.0` given the `0.14.0-RC1` release candidate.

Note that at the same time we will also publish the `0.15.0-RC1` release. We publish two releases at the same time as per the logic outlined at the [Example/At the Dotty Repo](#at-the-dotty-repo) and the [Model](#model) sections above: the step (5) in the algorithm outlined in the [Example](#at-the-dotty-repo) for the release cycle of `0.14.0` is the step (1) in the release cycle of `0.15.0`.

The following commands assume a remote tracking repository named `origin` pointing to the main Dotty repository: `https://github.com/lampepfl/dotty.git`.


```bash

######## Publish the 0.14.0 stable version – end the release cycle for 0.14.0 ########
git checkout 0.14.x

# Change `val baseVersion = "0.14.0-RC1"` to `val baseVersion = "0.14.0"` in project/Build.scala

git commit -am 'Release Dotty 0.14.0'
git tag 0.14.0
git push origin 0.14.0

git checkout master
git merge 0.14.x

# Make sure the merge doesn't break anything. In doubt, create a PR to run the CL
git push origin master

######## Publish the 0.15.0-RC1 unstable version – begin the release cycle for 0.15.0 ########
# Move all the unfinished tasks from Milestone 15 to Milestone 16 on GitHub – see https://github.com/lampepfl/dotty/milestones

git checkout -b 0.15.x

# Change val baseVersion = "0.15.0" to val baseVersion = "0.15.0-RC1"

git commit -am 'Release Dotty 0.15.0-RC1'
git tag 0.15.0-RC1
git push origin 0.15.x
git push origin 0.15.0-RC1

git checkout master

# Change val baseVersion = "0.15.0" to val baseVersion = "0.16.0" - this will be the next version after `0.15.0-RC1` is promoted to `0.15.0`.

git commit -am 'Set baseVersion to 0.16.0'
git push origin master
```