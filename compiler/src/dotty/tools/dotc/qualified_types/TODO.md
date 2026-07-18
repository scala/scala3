# TODO

## Assumption gathering (`ENode.assumptions`)

Design principle the mechanism converges on:

- **Types → pulled assumptions.** Facts about values (members, locals, parameters) are derived on demand from the types of the references occurring in the goal, closed transitively. This is order-independent, so it works with out-of-order typing (forward references, completers) where a linear log of "facts seen so far" does not. `Fact.Value` and the member-context machinery were removed in favor of this.
- **Control flow → pushed facts.** Only genuinely flow-sensitive knowledge (`Fact.Condition`, eventually `Fact.Case`) is recorded in the context. These are lexically scoped, method-local, and must keep snapshot semantics: deferred computations (completers forced mid-branch, `tryEither` attempts, inline expansion) capture a `Context` and must see exactly the facts of their lexical position — a mutable side stack would hand them the facts of whatever the typer happens to be doing when they run.

Known design debts to pay off:

1. **Termination and sharing are implicit.** `termAssumptions` converts a val's rhs and recurses into `assumptions` on it, with no visited set anywhere in the chain. Diamond-shaped val references duplicate work multiplicatively; termination rests on accidental properties of the program. Fix: make gathering a worklist over atoms with a visited set and a budget. This also makes the assembled environment deterministic, which the future SMT backend needs.

2. **Provability depends on `defTree`.** `termAssumptions` reads `tp.symbol.defTree`, which is best-effort: absent for symbols from other compilation units or after unpickling without retained trees, and phase-fragile (`ANF` has to restore defTrees so the solver keeps finding rhs's). The same program can prove different things under joint vs. separate compilation. Principled position: assumptions come from *types* (durable, pickled); rhs-equality is a deliberate *transparency* rule, opt-in per symbol, with an explicit pickling story.

3. **The self-reference guard conflates lexical position with proof dependency.** `selectAssumptions` drops a member's facts whenever `ctx.owner.ownersIterator` contains the member: over-conservative (unrelated obligations inside the member's body also lose its qualifier) and under-protective (mutual circularity between two members' qualifiers is not detected). The real invariant is "an obligation must not be discharged using itself": track an explicit in-progress set of members whose qualifiers are currently being established, threaded through solver queries.

4. **Two notions of "a type's qualifier facts".** `selectAssumptions.facts` is a near-copy of `typeAssumptions.rec` minus selfification, prefix recursion, and term assumptions. Unify into one parameterized traversal so the rule set lives in one place.

Suggested end state: write the environment judgment as a small set of inference rules (declared-qualifier, skolem-equality, rhs-transparency, result-type, prefix, minus in-progress) and shape the code so each rule is one function.
