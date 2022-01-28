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
    - Further minimising the issue or asking the reporter of the issue to minimize it correctly (or label “stat:needs minimization”)

Other core teammates are responsible for providing information to the Issue Tsar in a timely manner when it is requested if they have that information.

## Assignment
Issue Tsar is rotated according to a well-defined, ordered list. They are appointed for 7 days and are responsible for what is specified in the “Responsibilities” section during those 7 days. Their assumption of the role starts on Monday and ends on Friday (both inclusive), and the scope of their responsibilities extends to all events that took place during the weekend before the Monday of their assignment.

## Prerequisites
An issue tasr needs to have all the accesses and privileges required to get their job done. This might include:
Admin rights in lampepfl/dotty repository
Admin rights in lampepfl/dotty-feature-requests repository
Permissions to create new repositories in lampepfl organization (needed to fork repositories for the community build)

## Procedures
To ensure proper health of the infrastructure, the Tsar regularly monitors its proper operation. If a malfunction is detected, it is the Tsar's job to ensure that there’s someone working on it (or solve it on their own).

If it is unclear what area an issue belongs to, the Tsar asks for advice from other team members, on Slack or GitHub. If, after asking for advice, it turns out that nobody in the team knows how to classify it, the issue must be classified with a “stat:needs triage” label.

If it is unclear who should review an external PR, the Tsar asks for advice from the rest of the core team. If after asking for advice, it is still unclear who should do it, the reviewer for such a PR will be decided at the next Dotty meeting.

In general, if anything else is unclear for proper fulfilment of responsibilities, the Tsar must proactively seek advice from other team members on Slack or other channels.

## Reporting
At the end of their Tsardom, the Tsar reports to the team during the Dotty meeting on the following points:

- Whether there were any incidents with the CI, nightlies and benchmarks, how they were resolved and what steps were taken to prevent them from happening in the future.
- How many new external contributors’ PRs were there and what they were about (in brief).
- How many new issues were opened during their Tsardom period? Were there any areas that got a lot of issues? How many regressions from a prior Scala 3 release were there? Which were designated for an MSc project or an Issue Spree?
- If new labels were created or old ones were removed, or there is any other feedback on how to improve the Tsardom, mention that.
- Unassigned PRs and issues that the team failed to classify: bring them one by one so that the team can make a decision on them.
