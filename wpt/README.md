# WPT diagnostic harness

This harness adapts a subset of the Web Platform Tests to Lightning CSS. It is a diagnostic tool, not a claim of browser conformance. It runs locally on demand and in GitHub Actions. CI checks the corpus against reviewed existing failures in `baseline.json` and fails on new or changed failures. The harness's own tests run in a separate job.

The initial milestone implements property-value extraction, parser recognition, and browser checks for context-free parsing/shorthand helpers. It uses the pinned revision in `revision.json`; `fixtures.json` is generated locally or in CI and is not committed. The full current extraction contains 24,344 cases from 902 pages, plus records for 150 unsupported and 9 empty pages. `selection.json` retains the optional eight-file starter selection. Derived fixtures retain their upstream paths and source hashes; see `LICENSE-WPT.md`.

## Run

Use the repository's normal Rust/Node development setup, including Node 24+ as required by `package.json`. Python 3 is needed to regenerate fixtures and run the HTML reader tests. Browser checks use the existing Puppeteer dependency and an available Chrome executable.

```sh
# After generating fixtures (see below): parser/serializer/optimizer diagnostics.
yarn test:wpt

# Compare with reviewed existing failures (the CI regression check).
yarn test:wpt:baseline

# Include browser checks. Set this to a Chrome executable available on your OS.
WPT_CHROME='/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' yarn test:wpt:browser

# Focus on one property, input substring, source path, or case ID.
node wpt/run.mjs --filter column-gap --modes print --browser

# Test the harness itself (not WPT conformance).
yarn test:wpt:harness

# Include the browser oracle's synthetic regression tests.
WPT_BROWSER_TESTS=1 WPT_CHROME='/path/to/chrome' yarn test:wpt:harness
```

The runner always builds `examples/wpt_adapter.rs` from this checkout with Cargo, rather than loading the installed Node binding. It uses a persistent JSONL child process, catches Rust unwinds, and restarts after process crashes/timeouts. It records case identity before attributing the response, so a failing case does not terminate the rest of the corpus.

Output defaults to ignored `wpt/results.json`; change it with `--output PATH`. Exit codes:

- `0`: no findings, or all findings match the selected baseline exactly. This does **not** mean all cases have typed support or full behavioral validation.
- `1`: without a baseline, findings needing review; with a baseline, new or changed findings or obsolete expectations.
- `2`: harness/setup failure, crash, timeout, protocol error, or invalid/mismatched baseline. Caught execution errors leave a partial report with `complete: false` and `harnessError`. Isolated crashes/timeouts can leave a complete diagnostic matrix and still exit `2`.

Browser baseline failures, coverage gaps, contextual matches, and cases needing a richer fixture remain explicit diagnostic statuses. They are not silently counted as passes. Raw corpus diagnostics exit `1` for existing issues and unadapted pages. The reviewed baseline permits those exact findings while preserving their original statuses in JSON and HTML reports.

The four modes are `print`, `minify`, `lower`, and `lower-minify`. `print` only parses/prints. The other modes run declaration optimization; the `lower` modes additionally target Chrome 80. `lower` prints readable output and `lower-minify` prints compact output. This target is an initial stress configuration, not a browser support claim. The browser oracle runs original and emitted CSS in the same current browser, and records that browser's version/executable. `--modes print,minify` selects a smaller matrix.

## GitHub Actions

`.github/workflows/wpt.yml` runs on pull requests to `master`, pushes to `master`, and manual dispatch. The **WPT harness tests** job runs the Rust adapter and Node tests, including the browser oracle and HTML report tests, using Chrome installed on the Ubuntu runner.

The separate **WPT regression tests** job clones the revision from `wpt/revision.json` into the runner's temporary directory, extracts the full corpus with `--discovery`, and runs all four parser/transform modes against `baseline.json`. Browser checks are not enabled for this corpus run.

Both jobs fail the workflow on errors. Report generation and artifact upload run after test failures whenever results are available. Download and unzip the **wpt-report** artifact from the Actions run to get `results.html` and the underlying `results.json`; artifacts are retained for 14 days. Setup failures that produce no results remain visible in the job logs.

## Reviewed failure baseline

`baseline.json` registers exact existing parser failures, browser differences, and unadapted/empty pages. Every entry references a review reason. The initial parser review covers 301 invalid typed-acceptance candidates, 40 valid-input rejections, and four infinite-hue panic inputs in all four modes. It also records 159 extraction gaps and the three starter cases with confirmed browser differences. These are reviewed diagnostic findings, not a claim that every parser candidate changes browser behavior.

The baseline pins the WPT revision, lowering targets, and the hashes of the full and starter corpora. Each parser/browser expectation identifies its registered corpora, one case and explicit transform modes, with its exact status, emitted CSS, and error or browser comparison. Extraction expectations identify their corpora, one file and its exact status/reason. Wildcard modes, duplicate entries, missing review reasons, and infrastructure-error expectations are rejected.

New findings fail the check even if the total failure count stays the same. Changed outputs/errors also fail. When a failure stops occurring or a page becomes adapted, the now-obsolete expectation fails the check so it can be removed. `--filter` and `--modes` compare only the selected executions; extraction checks still cover all pages in the supplied corpus. JSON reports include the baseline comparison in `baseline` and counts in `summary.baseline`; the HTML report retains original classifications and includes the comparison under run metadata.

```sh
# Check one property using the full generated corpus.
yarn test:wpt:baseline --filter column-gap --modes print

# Check the starter selection with a browser and the same baseline.
node wpt/extract.mjs /tmp/lightningcss-wpt --output /tmp/wpt-starter.json
WPT_CHROME='/path/to/chrome' node wpt/run.mjs --fixtures /tmp/wpt-starter.json \
  --browser --baseline wpt/baseline.json
```

Before adding an expectation, inspect the pinned upstream case and the report's original input, expectation, emitted CSS/error, and browser baseline where applicable. Add only the reviewed case/modes and a specific review reason. Do not regenerate the baseline from all current failures. For a fix, remove only the obsolete entries or modes and rerun the affected checks. Changes to extraction, WPT revision, or lowering targets require a separate review of the corpus/configuration metadata and affected outcomes. Browser checks use the installed Chrome; a browser-version change can require review of its comparisons.

Coverage gaps and unresolved browser checks remain diagnostic statuses outside this failure gate. A reviewed panic is accepted only with its exact panic outcome. Process crashes, timeouts, protocol errors, incomplete runs, and harness/setup failures always exit `2` and cannot be baselined.

## Readable HTML reports

```sh
# Read wpt/results.json and write wpt/results.html.
yarn report:wpt

# Or choose the input and output explicitly.
node wpt/report.mjs wpt/discovery-results.json --output wpt/discovery-report.html
```

Open the HTML file directly in a browser. It is self-contained, works offline, and does not need a server. The summary counts executions separately from distinct cases; a case's transform modes appear together in its details. Parser and browser classifications remain separate, and clicking a classification filters the property list.

The default view includes failure signals, coverage gaps, and unresolved browser checks. Search by property, input, source, or case ID, or filter by mode/classification. Property groups start collapsed and appear 20 at a time, case details load on demand, and large properties load 50 cases at a time. Expanded cases include the original input, WPT expectation, emitted CSS, errors, browser comparisons, and a pinned upstream source link. Extraction diagnostics and run metadata have their own collapsed sections. Partial runs are clearly marked.

The HTML summarizes the supplied results file; it does not rerun tests or change failure policy. Generated default reports are ignored by Git.

## Refresh or expand the corpus

Generate `fixtures.json` before the first local run. A WPT checkout is only necessary for extraction. For a new checkout:

```sh
git clone --depth 1 --filter=blob:none --sparse https://github.com/web-platform-tests/wpt.git /tmp/lightningcss-wpt
git -C /tmp/lightningcss-wpt fetch --depth 1 origin b89af32bc8f42d678f444eb0703bca015ddcf240
git -C /tmp/lightningcss-wpt checkout --detach b89af32bc8f42d678f444eb0703bca015ddcf240
git -C /tmp/lightningcss-wpt sparse-checkout set css resources common tools

# Regenerate the full property/shorthand corpus used by the runner.
node wpt/extract.mjs /tmp/lightningcss-wpt --discovery --output wpt/fixtures.json
node wpt/run.mjs --modes print --output wpt/discovery-results.json

# Optionally extract the smaller starter selection to a separate file.
node wpt/extract.mjs /tmp/lightningcss-wpt --output /tmp/wpt-starter.json
```

Extraction requires the pinned HEAD and an unmodified CSS checkout. It parses HTML with Python's HTMLParser, then evaluates inline generators with collector helpers. Loops, template literals, shared variables, and relative URLs for the standard helpers work. Stable IDs use file, helper kind, property/value hash, and a duplicate occurrence suffix. `scriptLine` identifies the containing script's starting line, not the exact helper-call line.

Each page is atomic: an unsupported dependency, DOM access, callback comparator, selector/rule helper, variant, script type, unpaired surrogate, or evaluation error discards all its partial cases and records the reason. Empty pages are also explicit. Only the known helper scripts are replaced; arbitrary external scripts are never fetched or evaluated. Node's VM is an evaluation convenience with a time limit, **not a security boundary**; use trusted pinned upstream source.

Discovery currently extracts 24,344 calls from 902 pages, with 150 unsupported and 9 empty pages. These totals are helper calls, not browser assertions or passes. Unsupported helper families and options remain explicit extraction gaps. No source-file modifications or regex rewrites of CSS/JS are used to extract cases.

## Interpretation

The Rust adapter uses `Property::parse` and requires the complete value to be consumed. It constructs the declaration from the parsed property, so a setter input containing `;` or `!important` cannot inject a second declaration or change priority. Production fallback behavior is unchanged.

| Result                                     | Meaning                                                                                                                                           |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| `recognized`                               | A valid value reached a typed property parser.                                                                                                    |
| `value-coverage-gap`                       | A valid value was preserved as unparsed tokens.                                                                                                   |
| `property-coverage-gap`                    | A valid value used an unknown standard property.                                                                                                  |
| `generic` / `deferred` / `custom-property` | CSS-wide keywords, recognized variable/environment references, or author custom properties. These are not counted as missing standard properties. |
| `invalid-typed`                            | An invalid WPT value reached a typed parser. A validation candidate, not necessarily a behavioral change.                                         |
| `preserved-invalid`                        | Invalid input was retained; this is not proof of grammar validation.                                                                              |
| `valid-rejected` / `rejected`              | Parse/serialization returned an error for valid/invalid input.                                                                                    |
| `robustness-failure`                       | Panic, process crash, timeout, or protocol failure.                                                                                               |

Deferred classification is based on recognized token structure and is not a strict validation oracle for every substitution-containing grammar. Unknown standard properties remain visible even if their value is a CSS-wide keyword.

Browser checks first establish WPT's original specified-value expectation, including acceptable serialization arrays, shorthand longhands, unrelated-longhand checks, and valid-value round trips. Shorthand baselines also check `CSS.supports` and each expected longhand's serialization round trip. They then apply emitted declarations to a fresh style and compare longhands. Invalid input must remain invalid and leave a preceding valid fallback unchanged. Shorthands also receive a derived test with longhands preseeded to `inherit` to expose missing resets.

When CSSOM spelling differs, a derived fixed-size fixture compares the affected computed values. A `contextual-match` is evidence only for that fixture; `computed-difference` is a concrete observable difference. This prevents harmless color spelling changes from becoming failures while keeping longhand changes visible. URL/substitution-dependent output and source fixtures with styles or `#target` are deferred explicitly. The harness does not load arbitrary WPT pages in the browser at this stage.

Reports contain WPT and Lightning CSS revisions, dirty-tree metadata, adapter and corpus hashes, browser version, options, source inputs, emitted declarations, recognition, and comparison details. Browser versions are recorded, not downloaded/pinned automatically. Use the same executable version when comparing runs.

## First findings

The 122-case browser run found:

- `column-gap: 10` is browser-invalid but prints as valid `column-gap: 10px` in all four modes.
- Two animation shorthand cases lose duration, delay, timing function, and iteration-count values when an item has no animation name; the derived browser check exposes changed computed longhands. These need review in context before changing optimization behavior.
- One invalid color case fails in the original browser baseline; it is not attributed to Lightning CSS.
- Equivalent color spellings pass the contextual check. Lowered `light-dark()` output needs the surrounding fallback context and remains deferred.

After merging master at `cbeaf83`, the broader parser-only run caught panics for four infinite-hue inputs. It also reported 40 valid-input errors and substantial property/value coverage gaps. The exact reviewed failures are recorded in `baseline.json`.

## Next phases

1. **Implemented:** pinned property/shorthand corpus, extraction accounting, Rust recognition/printing/optimization, and crash isolation.
2. **Initial browser lane implemented:** context-free CSSOM checks, invalid fallback checks, shorthand reset probes, and derived computed comparisons. Extend this with original WPT fixtures, custom comparators, configurable targets, and more cascade sequences.
3. **Next:** selector/rule/media/recovery adapters and contextual computed-value tests under WPT's server.
4. **Later:** selected reftests with untransformed references and scheduled discovery.

The harness's own tests and the reviewed-failure regression check must continue to pass. Raw diagnostics remain available through `yarn test:wpt`.
