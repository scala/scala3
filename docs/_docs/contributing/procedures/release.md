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

### Canceling CI builds
**The below guidelines are needed only to speed up things. It is no mistake if you skip this section. However, if you do things wrong here, there may be trouble. So do it only if you feel yourself confident with the release cycle and the workings of the CI.**

Note that after the first stage of the release cycle (see "Publishing artifacts to Maven via CI" section of the checklist below) only three test runs are required to be run at the CI:

- `master` branch's latest *commit* with the updated `baseVersion`
- `<stable-version>` *tag* of the stable version being released
- `<rc-version>` *tag* of the RC version being released

However, you may end up with as many as 6 tasks being run. The auxiliary tasks may include:

- *commit* tests of the *tags* specified above. You may have two of these, corresponding to the two tags. You should see them appearing to have the same commit hash in the CI, but one of them will have the tag next to it and the other one will not. The *tag* one must remain, as the CI tasks on tags publish to maven. CI tasks on commits do not. So it is safe to cancel the task running on the commit, if the commit hash is the same as that of the tag's task commit.
- Older commit from the `master` branch. Look for all the tasks run on the `master` branch in the CI and see if there are more than one of these. Then, find the one testing the most recent commit of the branch. The others can safely be canceled.

## Documentation
### Release Procedure Checklist
Before we start the release procedure, we create an issue with a release checklist. As we go through the release, we update the checklist. To generate the checklist, run the following command:

`bash <(curl -sL https://raw.githubusercontent.com/lampepfl/dotty/master/docs/docs/contributing/checklist.sh) <stable_release>`

Above, `<stable_release>` is the stable version being released. For example, if you are releasing `0.14.0` and `0.15.0-RC1`, this variable is `14` and the command is as follows:

`bash <(curl -sL https://raw.githubusercontent.com/lampepfl/dotty/master/docs/docs/contributing/checklist.sh) 14`

Copy and paste the output into the release issue.

The ecosystem update section for some projects also mentions a set of criteria upon which the project is to be marked in the checklist. When the Travis build status is specified next to the project's name, it is to be understood that this build must pass after all of the other criteria of that project are checked. Note that due to caching, the label image next to the link may not reflect the real status of the build. Therefore, to verify the status, click on the link and make sure that your recent commit passes.

When no criteria is specified, common sense is to be used.

### GitHub Releases and Blog Post
After the release is done, we document it as follows:

- On the GitHub release page, modify the release drafts created by CI. The RC draft should include notable changes introduced since the previous RC. E.g. for `0.14.0-RC1` these are generated by `gren changelog -G --override -D prs --tags=0.13.0-RC1..0.14.0-RC1`. `gren` is available [here](https://github.com/github-tools/github-release-notes), and before running the above command, please make sure that (1) the `origin` branch points to the `lampepfl/dotty` repository and (2) the two tags mentioned in the command are pushed to the `master` branch of that repo. Otherwise, the command won't pick up the tags.
- Create a blog article documenting the most important changes done by the release.

## Ecosystem
After releasing a new version of Dotty, we need to make sure to update the following related projects:

- [Example Project](https://github.com/scala/scala3-example-project)
  - To deploy locally: `git clone https://github.com/scala/scala3-example-project.git && cd dotty-example-project/`
  - To test locally: `sbt run`
  - [Commit](https://github.com/scala/scala3-example-project/commit/76bf0b4d708206b1901fa7f291f07cd470506e79) updating the Dotty version (only `README` and `build.sbt` files)
- [Example Project with Mill](https://github.com/scala/scala3-example-project/tree/mill)
  - Deploy: `git clone https://github.com/scala/scala3-example-project && cd dotty-example-project && git checkout mill`
  - Test: `mill root.run`
  - [Commit 1](https://github.com/scala/scala3-example-project/commit/e1ad1905ef38d07943e0c176333ba24e306a2078) – `build.sc` only; [Commit 2](https://github.com/scala/scala3-example-project/commit/23bc5df89e72b782ab8e19157d6bbcb67eef30cd)
- [Dotty G8 template](https://github.com/scala/scala3.g8)
  - Deploy: `git clone https://github.com/scala/scala3.g8.git`
  - Test (note the relative path as argument to `sbt new`, hence this command should be run after Deploy in the same directory as Deploy): `sbt new file://./dotty.g8 --name=foo --description=bar && cd foo && sbt run`
  - [Commit](https://github.com/scala/scala3.g8/commit/0cde8fa843e15e916f07f22a196f35a5988b26af)
- [Dotty G8 template with cross build support](https://github.com/scala/scala3-cross.g8)
  - Deploy: `git clone https://github.com/scala/scala3-cross.g8.git`
  - Test: `sbt new file://./dotty-cross.g8 --name=foo --description=bar && cd foo && sbt run`
  - [Commit](https://github.com/scala/scala3-cross.g8/commit/0e3ea2ae8ba8d001e63e5098ff60d728036d358f)
- [Dotty Homebrew Formula](https://github.com/lampepfl/homebrew-brew)
  - Deploy: `git clone https://github.com/lampepfl/homebrew-brew.git && cd homebrew-brew`
  - [Commit](https://github.com/lampepfl/homebrew-brew/commit/04f7284564387754a360a354159f2f8d6156a6c7). SHA256 sum comes from the issue checklist computed for the release as specified above. The file with checksums is available at [GitHub Releases](https://github.com/lampepfl/dotty/releases) -> release in question -> assets -> `sha256sum.txt` -> `*.tar.gz` file sum.
- [Dotty test under various OSs](https://github.com/lampepfl/packtest)
  - Deploy: `git clone https://github.com/lampepfl/packtest.git && cd packtest`
  - [Commit](https://github.com/lampepfl/packtest/commit/6d3edf7333e0e788af7c4f4ab976b56905ddf9ed)
- [Scastie](https://github.com/scalacenter/scastie/)
  - Deploy (copy-paste the command and append the release id, e.g. `15` for `0.15.0-RC1`): `git clone https://github.com/scalacenter/scastie.git && cd scastie && git remote add staging https://github.com/dotty-staging/scastie && git checkout -b dotty-release-`<release_id>
  - [PR](https://github.com/scalacenter/scastie/pull/433) – push your changes to `staging` repo (as defined in "Deploy" above) with `git push -u staging`, then submit the PR from there.
- [Dotty](https://github.com/lampepfl/dotty/)
  - If you are reading this, Dotty should already be deployed on your machine :)
  - [PR](https://github.com/lampepfl/dotty/pull/6557)
- [Scalac](https://github.com/scala/scala)
  - Deploy: `git clone https://github.com/scala/scala.git && cd scala && git remote add staging https://github.com/dotty-staging/scala && git checkout -b dotty-release-`<release_id>
  - [PR](https://github.com/scala/scala/pull/7993)

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