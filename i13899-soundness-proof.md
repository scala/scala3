# Soundness of `matchAbstractTypeMember` Relaxation for Inline Parameters

**Status:** Paper-style proof. Not machine-checked.
**Target:** PR scala/scala3#26034 (fixes scala/scala3#13899)
**Reviewer concern:** `ip.T` (path-dependent type with an inline-parameter prefix) is not a well-formed type in DOT; admitting it in `TypeComparer.matchAbstractTypeMember` could open unsoundness holes.

This document gives a paper-style soundness argument by **translation** into vanilla DOT. We define a small inline-calculus `λ_inline` that models the relaxation, translate it into the standard DOT calculus of Rapoport et al. (OOPSLA 2017), and show that every λ_inline typing derivation maps to a DOT typing derivation of the same translated term. Soundness then follows from DOT's own soundness.

## 1. Background and notation

We follow the *simple soundness* DOT calculus of Rapoport, Kabir, He, Lhoták (OOPSLA 2017) [arXiv:1706.03814], which is the standard modern reference. We use the conventions:

- `Γ ⊢ t : T` for typing; `Γ ⊢ S <: T` for subtyping; `t ↦ t'` for small-step reduction.
- Variables `x, y, z`; paths `p ::= x | p.a` (we use only variable-headed paths for this argument, as in WadlerFest DOT).
- Type members written `{A: S..U}`; type alias `{A = T} ≡ {A: T..T}`.
- Path-dependent type `x.A` requires `x` to be a *stable* term — either a let-bound variable or a value-bound recursive self. The relevant DOT subtyping rules are:

```
  (Sel-<:)     Γ ⊢ x : {A: S..U}                  (<:-Sel)    Γ ⊢ x : {A: S..U}
              ─────────────────────                          ─────────────────────
                Γ ⊢ x.A <: U                                    Γ ⊢ S <: x.A
```

- The compiler's `matchAbstractTypeMember` corresponds to the symmetric pair of these rules: given a refinement `{A: T..T}` on a prefix of type `{A: S..U}`, check `S <: T <: U` *via* the projection `x.A` (whose `info` is `S..U`). Reading the Scala source against the DOT rules:

```scala
val ref1 = tp1.widenExpr.select(name)             // form  x.A
isSubType(rinfo2.lo, ref1) &&                     // S' <: x.A   (uses <:-Sel)
  isSubType(ref1, rinfo2.hi)                      // x.A <: U'   (uses Sel-<:)
```

The premise `tp1.isStable` (in the original code) is the syntactic side-condition `x ∈ stable(Γ)`. The relaxation extends `stable(Γ)` to admit inline parameters.

## 2. The source calculus λ_inline

`λ_inline` is DOT (Rapoport et al. 2017) extended with one new term form and one new typing rule.

### 2.1 Syntax

We partition term variables into two disjoint classes:

```
  x, y, z ∈ Var          -- regular variables (let-bound, recursive self)
  ip       ∈ InlineVar   -- inline-marked variables
  v        ::= ... (as in DOT)
  t        ::= ...               -- everything in DOT
            | inlet ip : T = t in t       -- new: inline binding
```

`inlet ip : T = t₁ in t₂` is the syntactic counterpart of an inline call site: it binds `ip` to the inline argument `t₁` in the body `t₂`, with the special property that `t₂` may form `ip.A` even though `ip` is not stable in the standard sense. There is no separate "inline def" form — call sites are inlined at the point of definition, so an inline def followed by `n` call sites is represented by `n` distinct `inlet` terms.

This is a faithful model of the Scala compiler's flow: `PrepareInlineable` type-checks the inline def body in a context where the parameter is in scope; each call site of the inline def expands to a fresh proxy binding (`Inliner.scala`'s `paramProxy`). The body's typing is what motivates the relaxation; the call-site expansion is what makes it sound.

### 2.2 Typing

We add one typing judgement marker: `Γ ⊢ᵢ t : T` denotes *typing in inline scope* — typing inside the body of an `inlet`. Outside any `inlet`, only the standard judgement `Γ ⊢ t : T` applies. Both judgements share all the standard DOT rules, with one exception below.

**Standard rules (unchanged).** All Rapoport et al. rules for variables, abstractions, application, let, object introduction, subsumption, and subtyping (including `Sel-<:` and `<:-Sel` with `x ∈ stable(Γ)`) are inherited by both `⊢` and `⊢ᵢ`.

**New rule (in `⊢ᵢ` only).**

```
                    (Sel-Inline)        ip : T ∈ Γ        Γ ⊢ᵢ T <: {A: S..U}
                                       ──────────────────────────────────────
                                            Γ ⊢ᵢ ip.A <: U      (and symmetrically Γ ⊢ᵢ S <: ip.A)
```

This is the relaxation: in inline scope, an inline-bound `ip` may appear as the prefix of a type member projection, and the subtyping rules for `ip.A` use the bounds *of the inline parameter's declared type*. The premise `ip : T ∈ Γ` requires `ip` to be the inline-marked variable bound by the enclosing `inlet`; it does *not* admit arbitrary unstable terms.

**The `inlet` rule.**

```
                    (T-Inlet)           Γ ⊢ t₁ : T          Γ, ip : T ⊢ᵢ t₂ : U          ip ∉ FV(U)
                                       ─────────────────────────────────────────────────────────────
                                                          Γ ⊢ inlet ip : T = t₁ in t₂ : U
```

The side condition `ip ∉ FV(U)` is essential: the result type cannot mention the inline-bound variable, because outside the `inlet` body `ip` will not be in scope. (Inside the body, intermediate types may mention `ip` — including types like `ip.A` — but they must be eliminated by the time we exit.)

This corresponds to the compiler's actual behavior: an inline def's *declared* return type cannot mention the parameter directly (or it would fail to be well-formed at the call site even after expansion). Inferred internal types like `Aux[F, ip.Context]` are erased into the final value via subtyping before they leak out.

### 2.3 Operational semantics

The only new reduction rule:

```
                    (E-Inlet)        ip$proxy fresh
                                    ──────────────────────────────────────────────────────────────
                                       inlet ip : T = v in t   ↦   let ip$proxy : T = v in t[ip ↦ ip$proxy]
```

Standard congruence rules let `t₁` reduce to a value before the `inlet` fires. After expansion, the binding becomes a standard `let`, and all occurrences of `ip` inside `t` become occurrences of the let-bound `ip$proxy` — which IS stable in DOT.

This rule mirrors `Inliner.bindArg` / `paramProxy` exactly: the inliner generates a fresh symbol, binds the call-site argument to it, and substitutes throughout the inline body. Crucially, the substitution `t[ip ↦ ip$proxy]` runs over *types as well as terms*, so an internal `ip.A` becomes `ip$proxy.A` — well-formed in DOT.

## 3. Translation to DOT

We define a translation `⟦·⟧ : λ_inline → DOT`. The translation is the identity on every form except `inlet`:

```
                    ⟦inlet ip : T = t₁ in t₂⟧  =  let ip : ⟦T⟧ = ⟦t₁⟧ in ⟦t₂⟧
                    ⟦ip.A⟧                      =  ip.A         -- (ip is let-bound in the target)
                    ⟦x⟧, ⟦v⟧, ⟦λx:T.t⟧, ...    =  homomorphic
                    ⟦Γ⟧                         =  pointwise on bindings
```

The translation **does not rename `ip`** — it relies on the fact that, in the target DOT term, `ip` is bound by `let`, which makes it stable and admits `ip.A` under the standard rules. This is the bookkeeping cost of the translation: we collapse the syntactic distinction between `inlet` (inline binding, source) and `let` (stable binding, target), and that collapse is exactly what's needed for `Sel-<:` / `<:-Sel` to apply.

## 4. Lemmas

### Lemma 1 (Stability under Translation).

*If `Γ, ip : T ⊢ᵢ t : U` in λ_inline, then in `⟦Γ⟧, ip : ⟦T⟧` (where `ip` is treated as a `let`-bound variable), `ip ∈ stable(⟦Γ⟧, ip : ⟦T⟧)`.*

**Proof.** Direct from the definition of `stable(·)` in Rapoport et al.: a variable is stable iff it is bound in the context by a `let` or by a recursive self type. The translation of `inlet ip : T = t₁ in t₂` introduces `ip` as a `let`-binding in the target. ∎

### Lemma 2 (Sel-Inline Translates to Standard Sel).

*If `Γ ⊢ᵢ ip.A <: U` is derivable in λ_inline by (Sel-Inline) from premises `ip : T ∈ Γ` and `Γ ⊢ᵢ T <: {A: S..U}`, then `⟦Γ⟧ ⊢ ip.A <: ⟦U⟧` is derivable in DOT.*

**Proof.** Let `Γ ⊢ᵢ T <: {A: S..U}` translate (by induction hypothesis on subtyping derivations, see Theorem 1) to `⟦Γ⟧ ⊢ ⟦T⟧ <: {A: ⟦S⟧..⟦U⟧}`. We have `ip : ⟦T⟧ ∈ ⟦Γ⟧`, so by subsumption `⟦Γ⟧ ⊢ ip : {A: ⟦S⟧..⟦U⟧}`. By Lemma 1, `ip ∈ stable(⟦Γ⟧)`. Apply standard DOT `Sel-<:`:

```
  ⟦Γ⟧ ⊢ ip : {A: ⟦S⟧..⟦U⟧}      ip ∈ stable(⟦Γ⟧)
  ─────────────────────────────────────────────────
                 ⟦Γ⟧ ⊢ ip.A <: ⟦U⟧
```

The symmetric case for `<:-Sel` is identical. ∎

### Lemma 3 (Proxy Substitution Lemma).

*If `Γ, ip : T ⊢ᵢ t : U` and `Γ ⊢ v : T` and `ip ∉ FV(U)`, then `Γ ⊢ t[ip ↦ ip$proxy] : U` in a context extended with `ip$proxy : T` as a let-binding.*

**Proof.** By induction on the typing derivation, exactly parallel to Rapoport et al. Lemma 5.1 (Substitution). The only non-standard case is when the derivation uses `Sel-Inline` on `ip.A`: after substitution, the term becomes `ip$proxy.A`, which is typable by standard `Sel-<:` / `<:-Sel` (since `ip$proxy` is `let`-bound, hence stable). All other cases are textually identical to the standard substitution lemma. ∎

This lemma is the formal counterpart of `Inliner.scala`'s `paramProxy` map and `InlinerMap.typeMap`: the substitution corresponds to the inliner's tree-and-type rewrite, and the lemma states exactly the property the operational soundness argument relies on.

## 5. Translation theorem

### Theorem 1 (Translation preserves typing).

*If `Γ ⊢ t : T` in λ_inline (or `Γ ⊢ᵢ t : T` in inline scope), then `⟦Γ⟧ ⊢ ⟦t⟧ : ⟦T⟧` in DOT.*

**Proof.** By mutual induction on the typing and subtyping derivations of λ_inline. Every standard rule is preserved because the translation is the identity on its term and type forms, and the corresponding DOT rule is available. The two non-standard cases:

**Case (Sel-Inline):** Discharged by Lemma 2.

**Case (T-Inlet):**

Premises: `Γ ⊢ t₁ : T`, `Γ, ip : T ⊢ᵢ t₂ : U`, `ip ∉ FV(U)`.
By IH: `⟦Γ⟧ ⊢ ⟦t₁⟧ : ⟦T⟧` and `⟦Γ⟧, ip : ⟦T⟧ ⊢ ⟦t₂⟧ : ⟦U⟧` (the inline-scope marker is dropped in the translation; the IH on the inline judgement justifies the standard DOT typing of `⟦t₂⟧`).

Note that since the translation reuses `ip` as the let-bound variable, and `ip ∉ FV(U)` so `ip ∉ FV(⟦U⟧)`, we can apply the standard DOT (T-Let):

```
  ⟦Γ⟧ ⊢ ⟦t₁⟧ : ⟦T⟧      ⟦Γ⟧, ip : ⟦T⟧ ⊢ ⟦t₂⟧ : ⟦U⟧     ip ∉ FV(⟦U⟧)
  ──────────────────────────────────────────────────────────────────────
                     ⟦Γ⟧ ⊢ let ip : ⟦T⟧ = ⟦t₁⟧ in ⟦t₂⟧ : ⟦U⟧
```

which is `⟦Γ⟧ ⊢ ⟦inlet ip : T = t₁ in t₂⟧ : ⟦U⟧` by the definition of `⟦·⟧`. ∎

### Corollary 1 (Reduction commutes with translation up to a let-rename).

*If `inlet ip : T = v in t  ↦  let ip$proxy : T = v in t[ip ↦ ip$proxy]` in λ_inline, then the translation of the RHS is the alpha-renaming `let ip$proxy : ⟦T⟧ = ⟦v⟧ in ⟦t⟧[ip ↦ ip$proxy]` in DOT, which is alpha-equivalent to the translation of the LHS.*

This is mechanical: both translations produce a `let` form; only the bound name differs, and `let` binders are alpha-convertible.

## 6. Soundness corollary

### Theorem 2 (Soundness of λ_inline).

*If `∅ ⊢ t : T` in λ_inline, then either `t` is a value, `t` diverges, or `t ↦* t'` for some `t'` with `∅ ⊢ t' : T`. Moreover, `t` cannot reduce to a stuck term.*

**Proof.** By Theorem 1, `∅ ⊢ ⟦t⟧ : ⟦T⟧` in DOT. By Theorem 3.20 of Rapoport et al. (OOPSLA 2017, soundness for DOT — progress and preservation), `⟦t⟧` does not get stuck and reduces only to values of type `⟦T⟧` or non-values that are still well-typed at `⟦T⟧`.

For reduction in λ_inline: every reduction in λ_inline that is not an `(E-Inlet)` step is a standard DOT reduction, preserved under translation. An `(E-Inlet)` step rewrites `inlet ip = v in t` to `let ip$proxy = v in t[ip ↦ ip$proxy]`; by Corollary 1, the translation of the RHS is alpha-equivalent to the translation of the LHS, so the DOT reduction sequence is unchanged. λ_inline cannot get stuck where DOT does not. ∎

## 7. Discussion: relationship to the compiler

This proof is a faithful operational model of:

- **The relaxation in `TypeComparer.matchAbstractTypeMember`** — corresponds to (Sel-Inline). The `stableEnoughForMember` predicate widens `stable(Γ)` to admit inline-marked variables; this exactly matches our `⊢ᵢ` judgement.

- **`PrepareInlineable` / inline def body typing** — corresponds to the `Γ, ip : T ⊢ᵢ t₂ : U` premise of (T-Inlet). The body is typed once, with the inline parameter in scope under the relaxed rule. The compiler's `widenExpr.select(name)` is the syntactic formation of `ip.A`.

- **`Inliner.bindArg` / `paramProxy`** — corresponds to the (E-Inlet) reduction. Each call site introduces a fresh stable proxy and substitutes throughout the inlined body.

- **`InlinerMap.typeMap`** — corresponds to the type-level component of the substitution `t[ip ↦ ip$proxy]` in (E-Inlet). The Substitution Lemma (Lemma 3) is the formal statement that this substitution is type-preserving — which is the invariant the operational argument informally relied on.

The side condition `ip ∉ FV(U)` in (T-Inlet) is what makes the system genuinely safe. In the Scala compiler, the analogue is the rejection of inline parameters in *declared* return types, parameter types, ascriptions, casts, and singleton positions — all of which would let `ip.A` escape its inline scope. These rejections are not part of the relaxation; they are enforced by `Checking.checkStable`, `TypeOps.isLegalPrefix`, and `NamedType`'s `validPrefix`, *all of which remain in force* after the relaxation. The negative test `tests/neg/i13899-bounds.scala` and the existing `tests/neg/inline-param-unstable-path.scala` pin down these boundaries.

## 8. What this proof does *not* cover

1. **Capture checking.** `λ_inline` does not model `cc`; the interaction between inline-param projection and capture sets is unstudied. The existing exception in `Checking.checkStable` for `Mode.InCaptureSet` already treats inline params as a special case; whether this interaction is sound under the full cc calculus is left to follow-up.

2. **Path-dependent types of length > 1.** `λ_inline` follows WadlerFest/simple-soundness DOT in restricting paths to variables. The pDOT extension (Rapoport-Lhoták 2019) would allow `ip.a.A`, but the relaxation in `TypeComparer` is restricted to `tp1: TermRef` (single hop), so the proof's scope matches the implementation's scope.

3. **Given resolution / implicit search interactions.** Implicit search may form types mentioning inline parameters during candidate filtering. The relaxation doesn't extend any rule used by implicit search; we believe the interaction is sound by the same translation argument, but have not checked it explicitly.

4. **Multi-step inlining.** The proof handles a single inline call site at a time. A nested inline def whose body inlines another inline def is handled by repeated application of (T-Inlet) — the proof composes — but we have not formalized the cross-`inlet` capture-avoidance bookkeeping.

5. **Machine-checked status.** This is a paper proof. It rephrases the operational argument in PR review as a translation theorem, gaining rigor from grounding in the published DOT soundness result, but it is not formalized in Coq/Agda/Iris. A mechanization would be tractable as an extension of the Rapoport et al. Coq development [github.com/amaurremi/dot-calculus]; we have not attempted it.

## 9. Position relative to prior work

The closest prior work is Stucki, Biboudis, Doeraene, Odersky, **"Semantics-Preserving Inlining for Metaprogramming"** (SCALA@SPLASH 2020). Stucki et al. give an inline calculus with proxy-introducing call-site expansion and a semantics-preservation theorem (inlined ≡ non-inlined). They do *not* treat type-member projection over inline parameters: their inline λ-calculus has simple types, no DOT-style refinements, and no `Sel-<:` / `<:-Sel` rules. This proof fills exactly that gap: it shows that the typing-time relaxation needed to make Scala's inline mechanism work with abstract type members of inline parameters is conservative over standard DOT typing post-expansion.

The structural analogue in the DOT literature itself is Rapoport et al.'s **tight typing**: tight typing admits derivations that fully general typing would too, but with a side condition that's only satisfiable at runtime configurations. Our (Sel-Inline) plays the same role for the typing-time / expansion-time split: it is admitted in `⊢ᵢ` (typing-time), and every derivation using it corresponds to a standard `Sel-<:` / `<:-Sel` derivation after the translation (which models expansion).

## Sources

- Rapoport, Kabir, He, Lhoták. *A Simple Soundness Proof for Dependent Object Types.* OOPSLA 2017. <https://arxiv.org/abs/1706.03814>
- Amin, Rompf. *Type Soundness Proofs with Definitional Interpreters.* POPL 2017. <https://arxiv.org/abs/1510.05216>
- Amin, Grütter, Odersky, Rompf, Stucki. *The Essence of Dependent Object Types.* WadlerFest 2016. <https://infoscience.epfl.ch/record/215280>
- Stucki, Biboudis, Doeraene, Odersky. *Semantics-Preserving Inlining for Metaprogramming.* SCALA 2020. <https://biboudis.github.io/papers/inlining-scala20.pdf>
- Stucki. *Scalable Metaprogramming in Scala 3.* EPFL PhD Thesis, 2021. <https://infoscience.epfl.ch/bitstreams/ebafddea-17ec-4626-a737-8d81f2e518ea/download>
- Rapoport, Lhoták. *A Path to DOT: Formalizing Fully Path-Dependent Types.* OOPSLA 2019. <https://arxiv.org/abs/1904.07298>
- Coq dev for Rapoport et al. 2017: <https://github.com/amaurremi/dot-calculus>
