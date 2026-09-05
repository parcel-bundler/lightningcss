# Opt-in WPT harness

This harness adapts a subset of the Web Platform Tests to Lightning CSS. It is a diagnostic tool, not a claim of browser conformance. It is deliberately **not connected to CI** and has no accepted-failure baseline yet.

The initial milestone implements property-value extraction, parser recognition, and browser checks for context-free parsing/shorthand helpers. It uses the pinned revision in `revision.json`; `fixtures.json` contains the full current extraction: 24,344 cases from 902 pages, plus records for 150 unsupported and 9 empty pages. `selection.json` retains the optional eight-file starter selection. Derived fixtures retain their upstream paths and source hashes; see `LICENSE-WPT.md`.

## Run

Use the repository's normal Rust/Node development setup. Node 18+ is needed for the harness tests. Python 3 is needed only to regenerate fixtures. Existing Puppeteer is used for browser checks; no new dependencies are installed.

```sh
# Parser/serializer/optimizer diagnostics, no WPT checkout or browser required.
yarn test:wpt

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

- `0`: no parser candidates, confirmed browser differences, or unadapted pages in the run. This does **not** mean all cases have typed support or full behavioral validation.
- `1`: findings needing review: typed acceptance of invalid input, rejection of valid input, robustness failures, browser differences, or unadapted/empty pages.
- `2`: harness/setup failure. A failure during execution also leaves a partial report with `complete: false` and `harnessError`.

Browser baseline failures, coverage gaps, contextual matches, and cases needing a richer fixture remain explicit diagnostic statuses. They are not silently counted as passes. The full corpus currently exits `1` because it finds existing issues and includes unadapted pages.

The four modes are `print`, `minify`, `lower`, and `lower-minify`. `print` only parses/prints. The other modes run declaration optimization; the `lower` modes additionally target Chrome 80. `lower` prints readable output and `lower-minify` prints compact output. This target is an initial stress configuration, not a browser support claim. The browser oracle runs original and emitted CSS in the same current browser, and records that browser's version/executable. `--modes print,minify` selects a smaller matrix.

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

A checkout is only necessary for extraction. The research checkout may still be available at `/private/tmp/lightningcss-wpt-research`. For a new checkout:

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

Discovery currently extracts 24,344 calls from 902 pages, with 150 unsupported and 9 empty pages. These totals are helper calls, not browser assertions or passes. They differ from the research probe because this implementation supports fewer helper families and rejects unsupported options instead of losing them during JSON serialization. No source-file modifications or regex rewrites of CSS/JS are used to extract cases.

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

Browser checks first establish WPT's original specified-value expectation, including acceptable serialization arrays, shorthand longhands, unrelated-longhand checks, and valid-value round trips. They then apply emitted declarations to a fresh style and compare longhands. Invalid input must remain invalid and leave a preceding valid fallback unchanged. Shorthands also receive a derived test with longhands preseeded to `inherit` to expose missing resets.

When CSSOM spelling differs, a derived fixed-size fixture compares the affected computed values. A `contextual-match` is evidence only for that fixture; `computed-difference` is a concrete observable difference. This prevents harmless color spelling changes from becoming failures while keeping longhand changes visible. URL/substitution-dependent output and source fixtures with styles or `#target` are deferred explicitly. The harness does not load arbitrary WPT pages in the browser at this stage.

Reports contain WPT and Lightning CSS revisions, dirty-tree metadata, adapter and corpus hashes, browser version, options, source inputs, emitted declarations, recognition, and comparison details. Browser versions are recorded, not downloaded/pinned automatically. Use the same executable version when comparing runs.

## First findings

The 122-case browser run found:

- `column-gap: 10` is browser-invalid but prints as valid `column-gap: 10px` in all four modes.
- Two animation shorthand cases lose duration, delay, timing function, and iteration-count values for an animation named `none`; the derived browser check exposes changed computed longhands. These need review in context before changing optimization behavior.
- One invalid color case fails in the original browser baseline; it is not attributed to Lightning CSS.
- Equivalent color spellings pass the contextual check. Lowered `light-dark()` output needs the surrounding fallback context and remains deferred.

The broader parser-only run caught nine panics, including infinite hue values and mixed number/percentage math for `opacity`. It also reported 40 valid-input errors and substantial property/value coverage gaps. These are diagnostic findings; neither production code nor expected-failure baselines were changed to make them pass.

## Next phases

1. **Implemented:** pinned property/shorthand corpus, extraction accounting, Rust recognition/printing/optimization, and crash isolation.
2. **Initial browser lane implemented:** context-free CSSOM checks, invalid fallback checks, shorthand reset probes, and derived computed comparisons. Extend this with original WPT fixtures, custom comparators, configurable targets, and more cascade sequences.
3. **Next:** selector/rule/media/recovery adapters and contextual computed-value tests under WPT's server.
4. **Later:** selected reftests with untransformed references, then a separately reviewed CI baseline and scheduled discovery.

Keep the opt-in behavior until findings and assertion policy have been reviewed. The design rationale and broader scope are in `../WPT-HARNESS-RESEARCH.md`.
