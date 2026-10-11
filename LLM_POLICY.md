# Policy regarding LLM-generated code in contributions to the Scala 3 compiler

# Foreword

The Scala 3 compiler accepts contributions containing code produced with AI assistance. This means that using LLM-based
tooling aiding software development (like Cursor, Claude Code, Copilot or whatever else) is allowed. The following
policy is meant to encourage and regulate responsible use of such tools and maintain a high quality of the end product 
(the compiler, and Scala as a language).   

LLM-powered tooling has great potential for speeding up software development, and has been adopted widely in the
ecosystem. That being said, its widespread use poses certain risks, which makes a document such as this one necessary.
We want to ensure this potential is used responsibly and remains sustainable in the long term.

This policy aims to strike the right balance: embracing LLM-assisted contributions, encouraging both newcomers and
experienced contributors to take full responsibility for the code they submit, and preserving code correctness while
keeping the maintainers’ workload manageable.

# Hard requirements

The following rules are non-negotiable and necessary for a contribution to be considered.

- All interactions (issues, pull requests, discussions, etc.) must be 100% human-written.
  LLM-generated text has to be processed and understood by a human, pasting LLM outputs verbatim is not allowed.
- The author of the contribution is responsible for every line of code, comment, documentation, test or decision
  contained in it, regardless if it was done by hand or generated automatically by tooling.
- Any potential changes to requirements from a bug tracker issue must be discussed under the issue, not suggested in a PR.
- The author of a PR must be capable of answering reviewer questions and explaining their solution in detail.
- Contributed code should be covered by automated tests. If test automation is not feasible, an explanation has to be
  given in the pull request description, including a description of exactly how the change was tested manually.
  - Tests are not enough evidence that a contribution is good all on their own. We also care about maintainability,
    and we acknowledge that our test coverage will never be 100% for edge cases, and so passing tests is not a guarantee of correctness.
- The contributed solutions have to have been tested by the contributor on their local machine before submitting for
  review.
    - Contributed code has to compile successfully.
    - Note: it is fine to keep solutions under development in draft format and ask maintainers for feedback.

# Guidelines

The following is a set of guidelines. Their purpose is to highlight what increases the quality of a
given contribution from the maintainers’ perspective.

- **State clearly in the pull request description, whether LLM-based tools were used and how their output was checked.**
- Code generated with an LLM needs to be reviewed and tested by the contributor in accordance with
  established best practices. Using an LLM is no excuse for bad code quality.
    - In particular, validation and tests should first be done on the author’s local machine, rather than submitted
      straight to the compiler CI.
- Overly verbose code comments may be useful for the AI agent to explain what it did, but it is not useful to commit
  them to a repository, as meant to be read and maintained by a human.
- If LLM-based tools were used extensively in a contribution, attaching prompt history in a comment is considered a
  useful aid for code reviewers.
- Do not open pull requests without the intention of answering code reviews and pushing it to its completion.
- Do not forward code review questions and other discussion to an LLM.
- Do not contribute large volumes of LLM-generated code all at once. Try to scope them down to something digestible for code reviewers.

# Examples

#### A contribution is likely to be accepted, if:

- The author of the contribution understands every line of code they contributed and can explain how it works and what
  purpose it serves.
- The code fulfills the requirements of the issue in the bug tracker it is meant to solve.
- The code is well scoped, well documented and covered with tests.
- The solution is clearly of high quality, regardless if it was coded manually or generated automatically.

#### A contribution is likely to be rejected with a warning, if:

- It contains low-quality LLM-generated code which, while fulfilling the requirements, would require multiple rounds of
  code review and a lot of work on the side of the maintenance team.
- The author does not understand pieces of code filled in by an LLM and cannot explain why the solution works, even if
  it does, in fact, work.

#### Further submissions may be restricted if:

- The author of the contribution does not understand the code they contributed.
- The contribution does not behave as described.
- The reviewer has doubts whether the author of the contribution is human or a bot.

# Additional notes

- This policy applies to all contributors. It may be enforced more strictly for first-time and relatively new
  contributors, who are expected to demonstrate familiarity with the guidelines before submitting substantial changes.
- Contributors may be temporarily restricted from further submissions if there is reasonable suspicion of repeated or
  serious non-compliance with this policy.
- Restrictions may increase with repeated violations. A typical progression may be one month, three months, one year,
  and, in severe or persistent cases, indefinite suspension.