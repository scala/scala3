# Issue Tsar Role
This document formally defines the Issue Tsar role. This is a repository maintenance role that is assigned to core contributors on rotating basis.

## Responsibilities
Issue Tsar is responsible for:
- Health of the CI, nightly releases and benchmark infrastructure.
- PRs of external contributors: assigning someone to review, or handling themselves.
- Triaging issues (especially new):
  - Each issue needs to be assigned an `itype` and 1 or more `area` labels.
  - Where applicable, new issues need to be designated for the Spree or Semester projects with the corresponding labels.
  - Regressions from an earlier Scala 3 release must be classified with the “regression” label and assigned to the next release’s milestone.
  - Modifying issue labels to best capture information about the issues
    - Attempting to reproduce the issue (or label “stat:cannot reproduce”)
    - Further minimizing the issue or asking the reporter of the issue to minimize it correctly (or label “stat:needs minimization”)

Other core teammates are responsible for providing information to the Issue Tsar in a timely manner when it is requested if they have that information.

## Assignment
Issue Tsar is appointed for 7 days and is responsible for what is specified in the “Responsibilities” section during those 7 days. Their assumption of the role starts from the Dotty Meeting on Monday and ends on the next Dotty Meeting on Monday.

During the Dotty Meeting, an Issue Tsar is assigned for the current week and for the week after that.

Issue Tsardom schedule is maintained in the [Issue Tsardom Statistics spreadsheet](https://docs.google.com/spreadsheets/d/19IAqNzHfJ9rsii3EsjIGwPz5BLTFJs_byGM3FprmX3E/edit?usp=sharing). So, someone who knows their availability several weeks ahead into the future can assign themselves to be an issue tsar well ahead of time.

## Prerequisites
An issue tsar needs to have all the accesses and privileges required to get their job done. This might include:
- Admin rights in lampepfl/dotty repository
- Admin rights in lampepfl/dotty-feature-requests repository
- Permissions to create new repositories in lampepfl organization (needed to fork repositories for the community build)
- Access to the LAMP slack to be able to ask for help with the infrastructure, triaging and such

## Procedures
To ensure proper health of the infrastructure, the Tsar regularly monitors its proper operation. If a malfunction is detected, the Tsar's job is to ensure that someone is working on it (or solve it on their own).

If it is unclear what area an issue belongs to, the Tsar asks for advice from other team members on Slack or GitHub. If, after asking for advice, it turns out that nobody in the team knows how to classify it, the issue must be classified with a “stat:needs triage” label.

If it is unclear who should review an external PR, the Tsar asks for advice from the rest of the core team. If after asking for advice, it is still unclear who should do it, the reviewer for such a PR will be decided at the next Dotty meeting.

In general, if anything else is unclear for proper fulfillment of responsibilities, the Tsar must proactively seek advice from other team members on Slack or other channels.

## Reporting
At the end of their Tsardom, the Tsar reports to the team during the Dotty meeting on the following points:

- Whether there were any incidents with the CI, nightlies and benchmarks, how they were resolved and what steps were taken to prevent them from happening in the future.
- How many new external contributors’ PRs were there and what they were about (in brief).
- How many new issues were opened during their Tsardom period? Were there any areas that got a lot of issues? How many regressions from a prior Scala 3 release were there? Which were designated for an MSc project or an Issue Spree?
- If new labels were created or old ones were removed, or there is any other feedback on how to improve the Tsardom, mention that.
- Unassigned PRs and issues that the team failed to classify: bring them one by one so that the team can make a decision on them.

# Maintenance List
The following is the list of all the principal areas of the compiler and the core teams members who are responsible for their maintenance:

- Typer: @odersky, @smarter, (@dwijnand)
- Erasure: @smarter, @odersky
- Enums: @bishabosha
- Pattern Matching: @dwijnand, (@liufengyun), @sjrd
- Inline: @nicolasstucki, @odersky
- Metaprogramming (Quotes, Reflect, Staging): @nicolasstucki, @aherlihy
- Match types: @OlivierBlanvillain, @dwijnand
- GADT: @abgruszecki, @dwijnand
- Scaladoc: @KacperFKorban,  @BarkingBad, @pikinier20
- Initialization checker: @olhotak, @liufengyun, @anatoliykmetyuk
- Safe nulls: @noti0na1, @olhotak
- tailrec: @sjrd, @mbovel
- JS backend: @sjrd
- forward compat (-scala-release): @prolativ, @Kordyjan, (@nicolasstucki)
- Benchmarks: @anatoliykmetyuk, @mbovel
- REPL: @dwijnand, @anatoliykmetyuk, @prolativ
- CI: @anatoliykmetyuk
- Community Build: @anatoliykmetyuk
- Vulpix: @dwijnand, @prolativ
- JVM backend: @Kordyjan, (@sjrd)
- Derivation & Mirrors: @bishabosha, (@dwijnand)
- Linting (especially unused warnings) / Reporting UX  : VirtusLab TBD?
- Java-compat: @Kordyjan
