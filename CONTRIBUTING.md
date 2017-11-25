# Thank you for contributing to Dotty

We follow the standard GitHub [fork & pull](https://help.github.com/articles/using-pull-requests/#fork--pull) approach to pull requests. Just fork the official repo, develop in a branch, and submit a PR!

You're always welcome to submit your PR straight away and start the discussion (without reading the rest of this wonderful doc, or the [`README.md`](README.md)). The goal of these guidelines is to make your experience contributing to Dotty as smooth and pleasant as possible. We're happy to guide you through the process once you've submitted your PR.

## Signing the CLA

Regardless of the nature of your Pull Request, we have to ask you to digitally sign the [Scala CLA](http://www.lightbend.com/contribute/cla/scala), to protect the OSS nature of the code base.

## General Workflow

This is the process for committing code to the Scala project. There are of course exceptions to these rules, for example minor changes to comments and documentation, fixing a broken build etc.

Before starting to work on a feature or a fix, it's good practice to ensure that:

  1. There is a ticket for your work in the project's [issue tracker](https://github.com/lampepfl/dotty/issues);
  2. The ticket has been discussed and prioritized by the team.

You should always perform your work in its own Git branch. The branch should be given a descriptive name that explains its intent. Some teams also like adding the ticket number and/or the [GitHub](http://github.com) user ID to the branch name, these details is up to each of the individual teams. (See below for more details on branch naming.)

## Opening a PR

  When the feature or fix is completed you should open a [Pull Request](https://help.github.com/articles/using-pull-requests) on GitHub. Please have a look at and follow the [Pull Request Policy](https://github.com/scala/scala/wiki/Pull-Request-Policy) for guidelines on submitting a pull request to the dotty project. (the pull request policy is the same as for the Scala programming language)

## Identifying the kind of PR

1. Documentation
    Whether you finally decided you couldn't stand that annoying typo anymore, you fixed the outdated code sample in some comment, or you wrote a nice, comprehensive, overview for an under-documented package, some docs for a class or the specifics about a method, your documentation improvement is very much appreciated, and we will do our best to fasttrack it.

    You can make these changes directly in your browser in GitHub, or follow the same process as for code. Up to you!

2. Code

    For bigger changes, we do recommend announcing your intentions on [gitter](https://gitter.im/lampepfl/dotty) first, to avoid duplicated effort, or spending a lot of time reworking something we are not able to change at this time in the release cycle, for example.


3. Bug Fix

    At the end of the commit message, include "Fixes dotty/bug#NNNN", where https://github.com/lampepfl/dotty/issues/NNNN tracks the bug you're fixing. We also recommend naming your branch after the ticket number.

4. Enhancement or New Feature

    For longer-running development, likely required for this category of code contributions, we suggest you include `topic/` or `wip/` in your branch name, to indicate that this is work in progress, and that others should be prepared to rebase if they branch off your branch.

    Any language change (including bug fixes) must be accompanied by the relevant updates to the spec, which lives in the same repository for this reason.

5. Work In Progress

    It is ok to work on a public feature branch in the GitHub repository. Something that can sometimes be useful for early feedback etc. If so, then it is preferable to name the branch accordingly. This can be done by either prefixing the name with ``wip-`` as in ‘Work In Progress’, or use hierarchical names like ``wip/..``, ``feature/..`` or ``topic/..``. Either way is fine as long as it is clear that it is work in progress and not ready for merge. This work can temporarily have a lower standard. However, to be merged into master it will have to go through the regular process outlined above, with Pull Request, review etc..

    Also, to facilitate both well-formed commits and working together, the ``wip`` and ``feature``/``topic`` identifiers also have special meaning.   Any branch labelled with ``wip`` is considered “git-unstable” and may be rebased and have its history rewritten.   Any branch with ``feature``/``topic`` in the name is considered “stable” enough for others to depend on when a group is working on a feature.


## Pull Request guidelines

### Tests are important

  Bug fixes should include regression tests -- in the same commit as the fix.

  If testing isn't feasible, the commit message should explain why.

  New features and enhancements must be supported by a respectable test suite.

  Some characteristics of good tests:

  * includes comments: what is being tested and why?
  * be minimal, deterministic, stable (unaffected by irrelevant changes), easy to understand and review
  * have minimal dependencies: a compiler bug test should not depend on, e.g., the Scala library

### Documentation

  This is of course required for new features and enhancements.

  Any API additions should include Scaladoc.

  Consider updating the package-level doc (in the package object), if appropriate.

### Coding standards

  Please follow these standard code standards, though in moderation (scouts quickly learn to let sleeping dogs lie):

  * Don't violate [DRY](http://programmer.97things.oreilly.com/wiki/index.php/Don%27t_Repeat_Yourself).
  * Follow the [Boy Scout Rule](http://programmer.97things.oreilly.com/wiki/index.php/The_Boy_Scout_Rule).

  Please also have a look at the [Scala Hacker Guide](http://www.scala-lang.org/contribute/hacker-guide.html) by @xeno-by.

### Clean commits, clean history

  1. If your work spans multiple local commits (for example; if you do safe point commits while working in a feature branch or work in a branch for long time doing merges/rebases etc.) then please do not commit it all but rewrite the history by squashing the commits into one large commit which is accompanied by a detailed commit message for (as discussed in the following sections). For more info, see the article: [Git Workflow](http://sandofsky.com/blog/git-workflow.html). Additionally, every commit should be able to be used in isolation-- that is, each commit must build and pass all tests.
  2. The first line should be a descriptive sentence about what the commit is doing. It should be possible to fully understand what the commit does by just reading this single line. It is **not ok** to only list the ticket number, type "minor fix" or similar. If the commit has a corresponding ticket, include a reference to the ticket number, prefixed with "Closes #", at the beginning of the first line followed by the title of the ticket, assuming that it aptly and concisely summarizes the commit in a single line. If the commit is a small fix, then you are done. If not, go to 3.
  3. Following the single line description (ideally no more than 70 characters long) should be a blank line followed by an enumerated list with the details of the commit.
  4. Add keywords for your commit (depending on the degree of automation we reach, the list may change over time):
      * ``Review by @githubuser`` - will notify the reviewer via GitHub. Everyone is encouraged to give feedback, however. (Remember that @-mentions will result in notifications also when pushing to a WIP branch, so please only include this in your commit message when you're ready for your pull request to be reviewed. Alternatively, you may request a review in the pull request's description.)
      * ``Fix/Fixing/Fixes/Close/Closing/Refs #ticket`` - if you want to mark the ticket as fixed in the issue tracker (Assembla understands this).
      * ``backport to _branch name_`` - if the fix needs to be cherry-picked to another branch (like 2.9.x, 2.10.x, etc)


  4. If a commit purely refactors and is not intended to change behaviour, say so.
  5. Backports should be tagged as "[backport]".

  Example:

      Closes #2 Fixes the build

        - Details 1
        - Details 2
        - Details 3


  Here is standard advice on good commit messages:
  http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html

### Pass all checks

  Our bot, dotty-bot, automatically checks whether all PR committers have signed the CLA and we validate our tests with the help of Drone. Please makes sure your PR passes both of these required checks.

## Reviews and changes

The Pull Request should be reviewed by other maintainers (as many as feasible/practical). Note that a reviewer can also be an outside contributor-- members of Typesafe and independent contributors are encouraged to participate in the review process. It is not a closed process. Please try to avoid conflict of interest -- the spirit of the review process is to evenly distribute the understanding of our code base across its maintainers as well as to load balance quality assurance. Assigning a review to a "sure win" reviewer is not a good long-term solution.

After the review, you should resolve issues brought up by the reviewers as needed (pushing a new commit to address reviewers' comments), iterating until the reviewers give their thumbs up, the "LGTM" (acronym for "Looks Good To Me").

Once the code has passed review, the Pull Request will be merged into the distribution.
