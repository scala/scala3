---
layout: doc-page
title: Sending in a pull request
redirectFrom: /docs/contributing/workflow/checklist.html
---

Once you solved the issue you were working on, you'll likely want to see your
changes added to the [Scala 3 repo][lampepfl/dotty]. To do that, you need to
prepare a [pull request][pull-request] with your changes. Assuming that the team
is aware of what you've been working, here are some final steps that you'll want
to keep in mind as you create your PR.

### 1. Sign the CLA

Make sure you have signed the [Scala CLA][cla]. If you have any questions about
what this is and why it's required you can read further about it [here][cla].

### 2. Make sure your work is on its own branch

When submitting your pull request it's always best to ensure the branch name is
unique to the changes you're working on. It's important not to submit your PR on
your `main` branch as this blocks maintainers from making any changes to your PR
if necessary.

### 3: Add Tests

Add at least one test that replicates the problem in the issue, and that shows it is now resolved.

You may of course add variations of the test code to try and eliminate edge cases.
[Become familiar with testing in Scala 3](./testing.md).

### 4: Add Documentation

Please ensure that all code is documented to explain its use, even if only internal
changes are made. This refers to scaladocs and also any changes that might be
necessary in the reference docs.

### 5: Double check everything

Here are a couple tips to keep in mind.

- [DRY (Don't Repeat Yourself)][dry]
- [Scouts Rule][scouts]
- When adding new code try use [optional braces]. If you're rewriting old code,
  you should also use optional braces unless it introduces more code changes
  that necessary.

### 6: Commit Messages

Here are some guidelines when writing commits for Dotty.

1. If your work spans multiple local commits (for example; if you do safe point
   commits while working in a feature branch or work in a branch for long time
   doing merges/rebases etc.) then please do not commit it all but rewrite the
   history by squashing the commits into one large commit which is accompanied
   by a detailed commit message for (as discussed in the following sections).
   For more info, see the article: [Git Workflow][git-workflow]. Additionally,
   every commit should be able to be used in isolation—that is, each commit must
   build and pass all tests.

2. The first line should be a descriptive sentence about what the commit is
   doing. It should be possible to fully understand what the commit does by just
   reading this single line. It is **not ok** to only list the ticket number,
   type "minor fix" or similar. If the commit has a corresponding ticket,
   include a reference to the ticket number, prefixed with "Closes #", at the
   beginning of the first line followed by the title of the ticket, assuming
   that it aptly and concisely summarizes the commit in a single line. If the
   commit is a small fix, then you are done. If not, go to 3.

3. Following the single line description (ideally no more than 70 characters
   long) should be a blank line followed by an enumerated list with the details
   of the commit.

4. Add keywords for your commit (depending on the degree of automation we reach,
   the list may change over time):
    * ``Review by @githubuser`` - will notify the reviewer via GitHub. Everyone
      is encouraged to give feedback, however. (Remember that @-mentions will
      result in notifications also when pushing to a WIP branch, so please only
      include this in your commit message when you're ready for your pull
      request to be reviewed. Alternatively, you may request a review in the
      pull request's description.)
    * ``Fix/Fixing/Fixes/Close/Closing/Refs #ticket`` - if you want to mark the
      ticket as fixed in the issue tracker (Assembla understands this).
    * ``backport to _branch name_`` - if the fix needs to be cherry-picked to
      another branch (like 2.9.x, 2.10.x, etc)

Example:

```
fix: here is your pr title briefly mentioning the topic

Here is the body of your pr with some more information
  - Details 1
  - Details 2
  - Details 3

Closes #2
```

#### Skipping parts of CI

Depending on what your PR is addressing, sometimes it doesn't make sense to run
every part of CI. For example, maybe you're just updating some documentation and
there is no need to run the community build for this. We skip parts of the CI by
utilizing keywords inside of brackets. The most up-to-date way to see this are
by looking in the `if` statements of jobs. For example you can see some
[here](https://github.com/lampepfl/dotty/blob/5d2812a5937389f8a46f9e97ab9cbfbb3f298d87/.github/workflows/ci.yaml#L54-L64).
Below are commonly used ones:


|---------------------------|----------------------------------------|
| `[skip ci]`               | Skip the entire CI                     |
| `[skip community_build]`  | Skip the entire community build        |
| `[skip community_build_a]`| Skip the "a" community build           |
| `[skip community_build_b]`| Skip the "b" community build           |
| `[skip community_build_c]`| Skip the "c" community build           |
| `[skip docs]`             | Skip the scaladoc tests                |
| `[skip test]`             | Skip the unit tests                    |
| `[skip test_windows_fast]`| Skip the unit tests subset on Windows  |
| `[skip mima]`             | Skip the MiMa checks                   |
| `[skip test_sbt]`         | Skip the SBT scripted tests            |


#### Executes parts of the CI that are skipped on PRs
Depending on what your PR is addressing, sometimes it doesn't make sense to run
parts of the CI that usually ony runs on nightly builds.

|-------------------------------|---------------------------------------------------------------------------|
| `[test_java8]`                | Execute unit tests on Java 8                                              |
| `[test_windows_full]`         | Execute unit tests on Windows                                             |
| `[test_non_bootstrapped]`     | Execute unit tests using non-bootstrapped compiler                        |
| `[test_scala2_library_tasty]` | Execute unit tests using bootstrapped-compiler with Scala 2 library TASTy |

### 7: Create your PR!

When the feature or fix is completed you should open a [Pull
Request](https://help.github.com/articles/using-pull-requests) on GitHub.

If you're not actually finished yet and are just looking for some initial input
on your approach, feel free to open a [Draft PR][draft]. This lets reviewers
know that you're not finished yet. It's also a good idea to put a [wip] in front
of your PR title to make this extra clear.

Shortly after creating your pull request a maintainer should assign someone to
review it. If this doesn't happen after a few days, feel free to ping someone on
the [Scala Contributors Discord][discord] or tag someone on the PR. Depending on
the type of pull request there might be multiple people that take a look at your
changes. There might also be community input as we try to keep the review
process as open as possible.

### 8: Addressing feedback

More than likely you'll get feedback from the reviewers, so you'll want to make
sure to address everything. When in doubt, don't hesitate to ask for
clarification or more information.

Once you finally see the "LGTM" (Looks Good To Me or Let's Get This Merged)
you're PR will be merged in!

[pull-request]: https://docs.github.com/en?query=pull+requests
[lampepfl/dotty]: https://github.com/lampepfl/dotty
[cla]: http://typesafe.com/contribute/cla/scala
[issues]: https://github.com/lampepfl/dotty/issues
[full-list]: https://github.com/lampepfl/dotty/blob/master/CONTRIBUTING.md
[discord]: https://discord.gg/TSmY9zkHar
[dry]: https://www.oreilly.com/library/view/97-things-every/9780596809515/ch30.html
[scouts]: https://www.oreilly.com/library/view/97-things-every/9780596809515/ch08.html
[optional-braces]: https://docs.scala-lang.org/scala3/reference/other-new-features/indentation.html
[draft]: https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests#draft-pull-requests
[git-workflow]: http://sandofsky.com/blog/git-workflow.html
