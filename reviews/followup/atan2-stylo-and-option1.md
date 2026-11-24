# atan2(): what stylo does, and option 1 prototype

Branch: `claude/proto-atan2-single-parse` (commit 0431a5dc, on top of `claude/followup-cleanups`). Pushed; no PR opened.

## 1. Stylo

Stylo parses a math expression **once** into an untyped tree, `CalcNode = GenericCalcNode<Leaf>`. Each leaf keeps its own type. The type check runs afterwards on the tree, not during parsing. atan2 is never parsed once per candidate type.

Sources: servo/stylo `main` (identical to the `stylo` 0.24.0 crate, 2026-11-11), and 0.16.0 (2026-04) for the older `CalcUnits` design the question mentions.

### Current main: typed-OM `NumericType`

- **Leaf enum** ([specified/calc.rs#L111-L130](https://github.com/servo/stylo/blob/main/style/values/specified/calc.rs#L111-L130)): `Length(NoCalcLength)`, `Angle`, `Time`, `Resolution`, `ColorComponent`, `Percentage(CalcPercentageLeaf)`, `Number`, `TreeCountingFunction`, `RandomKey`.
- **`parse_one`** ([#L906-L945](https://github.com/servo/stylo/blob/main/style/values/specified/calc.rs#L906-L945)) accepts **any** dimension, whatever the context:
  ```rust
  &Token::Dimension { value, ref unit, .. } => {
      if let Ok(l) = NoCalcLength::parse_dimension_with_context(context, value, unit) {
          return Ok(CalcNode::Leaf(Leaf::Length(l)));
      }
      if let Ok(a) = NoCalcAngle::parse_dimension(value, unit) { return Ok(CalcNode::Leaf(Leaf::Angle(a))); }
      ... Time, Resolution ...
  ```
  Percentages are the one context-dependent part. `CalcParseFlags.percentage_context` ([#L316-L372](https://github.com/servo/stylo/blob/main/style/values/specified/calc.rs#L316-L372)) is either `NotAllowed` or `Allowed(percent hint)`. A `%` token either errors or becomes `Leaf::Percentage(CalcPercentageLeaf::new(v, hint))`.
- **atan2** ([#L1177-L1183](https://github.com/servo/stylo/blob/main/style/values/specified/calc.rs#L1177-L1183)):
  ```rust
  MathFunction::Atan2 => {
      let a = Self::parse_argument(context, input, flags)?;
      input.expect_comma()?;
      let b = Self::parse_argument(context, input, flags)?;
      require_consistent_type!(a, b);
      Ok(Self::Atan2(Box::new(a), Box::new(b)))
  },
  ```
  `require_consistent_type!` ([#L1002-L1027](https://github.com/servo/stylo/blob/main/style/values/specified/calc.rs#L1002-L1027)) computes `NumericType::add_two_types(a.numeric_type()?, b.numeric_type()?)` and then `as_calc_type()`. Each node's type comes from `CalcNode::numeric_type()` ([generics/calc.rs#L707-L856](https://github.com/servo/stylo/blob/main/style/values/generics/calc.rs#L707-L856)), which folds the leaf types upward. For `Atan2` it checks that the two arguments are addable, then returns `NumericType::angle()` ([#L787-L792](https://github.com/servo/stylo/blob/main/style/values/generics/calc.rs#L787-L792)).
- `CalcNode::Atan2` is kept as a node in the tree and is evaluated in `resolve_internal` ([generics#L1525-L1529](https://github.com/servo/stylo/blob/main/style/values/generics/calc.rs#L1525-L1529)), so arguments like `1px, 1em` work at computed-value time.
- **The target type is checked only at the top level.** `parse_angle` / `parse_length` / … call `Self::parse(...)` and then `.into_angle()` / `.into_length()`, which run `simplify_and_sort()` and check `numeric_type_as_calc_type() == CalcType::Angle` ([#L1550-L1562](https://github.com/servo/stylo/blob/main/style/values/specified/calc.rs#L1550-L1562), [#L1690-L1703](https://github.com/servo/stylo/blob/main/style/values/specified/calc.rs#L1690-L1703)).

### Older design (≤ 0.16): `CalcUnits` bitflags

Source: [docs.rs stylo 0.16.0 specified/calc.rs](https://docs.rs/crate/stylo/0.16.0/source/values/specified/calc.rs).

- `AllowParse { units: CalcUnits, additional_functions }` (L200-L235). `parse_one` only tried a dimension kind if `allowed.includes(CalcUnits::LENGTH/ANGLE/...)` (L615-L650).
- Each leaf reports `fn unit() -> CalcUnits` (L231). `CalcNode::unit()` ([generics, L488+](https://docs.rs/crate/stylo/0.16.0/source/values/generics/calc.rs)) ORs the units through sums, using `can_sum_with`. Products allow at most one non-number factor.
- atan2 (L866-L904) widened the allowed units and parsed each argument **once**. It then resolved eagerly by checking the type of the already-parsed tree:
  ```rust
  MathFunction::Atan2 => {
      let allow_all = allowed.new_including(CalcUnits::ALL);
      let a = Self::parse_argument(context, input, allow_all)?;
      input.expect_comma()?;
      let b = Self::parse_argument(context, input, allow_all)?;
      let radians = Self::try_resolve(input, || {
          if let Ok(a) = a.to_number() { let b = b.to_number()?; return Ok(a.atan2(b)); }
          if let Ok(a) = a.to_percentage() { let b = b.to_percentage()?; return Ok(a.atan2(b)); }
          if let Ok(a) = a.to_time(None) { ... }
          if let Ok(a) = a.to_angle() { ... }
          if let Ok(a) = a.to_resolution() { ... }
          let a = a.into_length_or_percentage(AllowedNumericType::All)?;
          let b = b.into_length_or_percentage(AllowedNumericType::All)?;
          let (a, b) = CalcLengthPercentage::same_unit_length_as(&a, &b).ok_or(())?;
          Ok(a.atan2(b))
      })?;
      Ok(Self::Leaf(Leaf::Angle(Angle::from_radians(radians))))
  }
  ```
  Here the "try each type" loop runs over a parsed tree (`to_number`, `to_angle`, …), not over the token stream. It costs O(size), with no re-parse.

**var():** stylo never sees `var()` in calc. A declaration that contains `var()` is stored as an unparsed token stream and substituted at computed-value time. Only after that substitution is the value parsed as calc, so there is no `var()` leaf.

## 2. Why lightningcss retries and stylo doesn't

lightningcss's `Calc<V>` is generic over the **target** type. Leaves are `Calc::Value(Box<V>)` or `Calc::Number`, and `V::parse` decides which dimension tokens are accepted. The type check is therefore done by the parser itself. A `Calc<Length>` cannot even hold an angle leaf. This is good for the AST: it is typed, and serialization and minification work per type. But atan2 accepts any type, as long as both arguments share it. With no type-erased representation, the only way to "check the type" was to parse the arguments as `Calc<Length>`, then `Calc<Percentage>`, `Calc<Angle>`, `Calc<Time>`, then `Calc<CSSNumber>`, and keep the first parse that succeeds.

A nested atan2 always yields an angle. The `<length>` and `<percentage>` attempts therefore fail only after fully parsing (5 attempts of) the inner atan2, which gives about 3–5^depth work. Stylo's leaves carry their own type, so one parse is enough, and the type check is a cheap fold over the tree.

## 3. Prototype (option 1)

`Calc::parse_atan2` now parses each argument once as `Calc<Atan2Arg>`. `Atan2Arg` is a private, type-erased leaf enum in `calc.rs`, the analogue of stylo's `Leaf`:

```rust
enum Atan2Arg { Length(Length), Percentage(Percentage), Angle(Angle), Time(Time), Mixed }
```

- `Parse` tries `Length::parse`, `Percentage::parse`, `Angle::parse`, then `Time::parse` on a single leaf token. These are disjoint, so the cost is constant per leaf. Numbers and constants stay `Calc::Number` as before.
- The trait impls that `Calc<V>` needs (`AddInternal`, `TryOp`, `TryMap`, `TrySign`, `PartialOrd`, `Mul<f32>`, `Into/TryFrom<Calc<_>>`, `From<Angle>`, `TryFrom<Atan2Arg> for Angle`) **delegate to the inner type when both sides have the same variant**, using a small `match_same_type!` macro. Every operation on leaves of one type therefore does exactly what `Calc<that type>` did before, so results for valid input don't change.
- If the variants differ, the operation returns `None` (no fold), or `Mixed` for `AddInternal::add`, which can't fail. atan2 then rejects the result and leaves the function as written, as before.
- `Into<Calc<Atan2Arg>>` unwraps `Length::Calc(Calc::Value(v))`, mirroring `Into<Calc<Length>>`. Without this, `calc(1px + 1em - 1em)` would not fold; the existing test caught it.
- A nested atan2 inside the arguments returns an `Angle`, which becomes `Atan2Arg::Angle` through `From<Angle>`. Single pass.
- `parse_ident` (relative color channels) is passed through for numbers only, as the old `<number>` attempt did.
- The final fold is the same as before: `(Value, Value)` → `try_op_to(atan2)`, `(Number, Number)` → `atan2`, anything else → error. The existing code never keeps an `atan2()` function in the AST (it errors and falls back to the unparsed value), so no new `MathFunction` variant or serialization was needed.
- Also included, taken from the cache branch: the `parse_value` change that stops rewinding after a failed nested math function before returning its error. Without it, var-containing nests were still quadratic: each enclosing block re-tokenized the failed function while skipping it. Example: coloratan2 at d=200 took 24 ms without it, 6 ms with it.
- No thread-locals, caches or new public API. The AST and serde output are unchanged.

**Diff vs `claude/followup-cleanups`:** `src/values/calc.rs` +170/−32, `src/lib.rs` +85 (tests).
For comparison, the cache branch is calc.rs +135/−12 and lib.rs +71. The prototype has more lines, but almost all of them are mechanical trait delegation in one self-contained private type. It has no global state and no reasoning about parse positions, cache invalidation or "typed attempt in progress" counters.

## Tests

- `cargo test --lib`: 132 passed. This includes `test_atan2_nested_parse_time` and the extra `test_trig` cases copied from `origin/claude/fix-nested-atan2`, plus 4 new assertions for the behavior changes below.
- `test_bundle` failed once in an early full run and passed on every rerun. It looks flaky and is unrelated.

## Equivalence (scripts/cases.mjs vs master build)

I ran the 20 original cases in `cases.css` plus 3 new ones (`cases-extra.css`; the combined file is `cases-all.css`): 68 runs.

- **All 20 original cases: output identical to master** (50 ok; the still-broken ones are pre-existing and unchanged).
- New case "mixed types (invalid)", **fixed**: `atan2(1 + 1px - 1px, 1)` was folded to `45deg` on master. The `<length>` attempt let the length cancel out of an invalid number + length sum. It is now left as written, which Chromium agrees with (the declaration is invalid). Other mixed inputs (`1px + 1deg`, `min(1px, 1deg)`, `10% + 1px`, `1s` vs `1px`) are unchanged and left as written.
- New case "channel times unit", **changed but equivalent**: `hsl(from red atan2(s * 1px, 1px) s l)` and `atan2(s * 1deg, l * 1deg)` are now folded (`#82ff00` and `#f0ff00`), and Chromium computes the same colors. On master only the `<number>` attempt passed channel keywords, so these stayed unparsed.
- New case "same-type sums" (`1px + 1in`, `1deg + 1rad`, `1s - 500ms`, `10% * 2 - 5%`, `calc(1px + 1em) - 1em`, `min(1px, 1em) + 1px`): output identical to master.
- Pre-existing, not addressed here: percentages are folded in `rotate`, where Chromium rejects them ("atan2 percentages", "trig inside", "nested both").

## Timings (min of 7 runs, ms, `--minify`)

| shape | depth | master | option 1 | cache branch |
|---|---|---|---|---|
| atan2 (valid, first arg) | 12 | 127 | 2 | 3 |
| atan2 | 14 | 1428 | 3 | 3 |
| atan2 | 200 | – | 3 | 3 |
| atan2sin | 14 | 2836 | 3 | 2 |
| atan2var | 10 | 7843 | 3 | 3 |
| atan2var | 200 | – | 5 | 5 |
| atan2sinvar | 10 | 9880 | 3 | 3 |
| atan2sinvar | 200 | – | 6 | 6 |
| atan2calcvar | 200 | – | 11 | 12 |
| coloratan2 | 14 | >30000 (timeout) | 3 | 3 |
| coloratan2 | 200 | – | 6 | 7 |
| coloratan2var | 200 | – | 6 | 7 |
| hypotvar | 200 | – | 5 | 4 |

Option 1 matches the cache branch everywhere, and output is identical between the two at every depth tested. Depth 800 hits cssparser's `MaximumNestingDepth` on all builds. `atan2calcvar` is still mildly superlinear on both, and the cause is outside atan2.

## Trade-offs

**Pros**
- Linear by construction, with no global or thread-local state and nothing to invalidate.
- Same shape as stylo: typed leaves, one parse, then a type check.
- Valid-input results are unchanged, because same-type operations delegate to the existing `Length`/`Percentage`/`Angle`/`Time` code.
- It fixes one wrong fold (`1 + 1px - 1px`).

**Cons**
- About 150 lines of delegation boilerplate for one function.
- The `Mixed` sentinel exists only because `AddInternal::add` is infallible.
- `TryFrom<Calc<Atan2Arg>>` is strict, like `Angle`/`Time`, while `Length` wraps sums. This can only change *where* an invalid atan2 fails, not whether it fails.
- Two observable changes, both towards the spec and confirmed in Chromium: relative color channels times a dimension now fold, and `1 + 1px - 1px` no longer folds. Error locations and kinds for invalid atan2 can differ slightly from master; the CSS output is the same.

**Possible later step:** the same type-erased enum (extended with `Number` and `Resolution`) could become a general untyped calc tree, as in stylo. That would remove the per-type retry from other places too, but it is a much larger refactor of `Calc<V>`.
