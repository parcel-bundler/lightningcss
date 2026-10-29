# WPT harness validation

This records the local validation on 2026-10-29 after merging master at
`cbeaf83b76b2113b3c4024931116cdca027f8385` into PR #1328. These findings are
diagnostics, not an accepted-failure baseline.

## Environment and scope

- WPT revision: `b89af32bc8f42d678f444eb0703bca015ddcf240`.
- Platform: macOS, arm64; Node `24.11.0`; Yarn `1.22.22`.
- Rust: installed stable `1.97.1`. The local `1.96.0` toolchain could not run
  Cargo, so the commands below selected stable with `RUSTUP_TOOLCHAIN=stable`.
  CI also selects stable explicitly.
- Browser: Chrome `156.0.8078.73`, supplied through `WPT_CHROME`.
- Corpus diagnostics used all four modes. Browser diagnostics used the
  eight-file starter selection, not the full corpus or original WPT pages.

## Reproduce the runs

Install the repository's dependencies and create the pinned WPT checkout as
described in [README.md](README.md). Set `WPT_CHECKOUT` to that checkout and
`WPT_CHROME` to the Chrome executable to use.

```sh
RUSTUP_TOOLCHAIN=stable WPT_BROWSER_TESTS=1 yarn test:wpt:harness

node wpt/extract.mjs "$WPT_CHECKOUT" --discovery --output wpt/fixtures.json
RUSTUP_TOOLCHAIN=stable node wpt/run.mjs --output wpt/discovery-results.json
node wpt/report.mjs wpt/discovery-results.json --output wpt/discovery-report.html

node wpt/extract.mjs "$WPT_CHECKOUT" --output /tmp/wpt-starter.json
RUSTUP_TOOLCHAIN=stable node wpt/run.mjs --fixtures /tmp/wpt-starter.json \
  --browser --output wpt/results-browser.json
node wpt/report.mjs wpt/results-browser.json --output wpt/results-browser.html
```

Both diagnostic runs exited `1` with complete reports. The harness tests
passed: three Rust tests and thirteen Node tests, with no skipped tests.
The browser tests cover baseline errors, alternative serializations,
shorthand expectations, changed values, missing resets, and report interaction.

The browser oracle's baseline checks were also compared with the pinned upstream
`parsing-testcommon.js` and `shorthand-testcommon.js` helpers for all 122 starter
cases. The upstream helpers ran in Chrome with synchronous assertion callbacks.
Their baseline success/failure outcomes matched the oracle for every case,
including the one invalid-color baseline failure. This comparison did not run
the original HTML pages or establish behavior in other browsers.

## Full parser/transform run

Extraction produced 24,344 cases from 902 pages. Another 150 pages were
unsupported and nine were empty. All 97,376 mode executions completed.

- `robustness-failure`: 16 executions from four cases, all infinite hue inputs.
- `valid-rejected`: 160 executions from 40 cases.
- `invalid-typed`: 1,204 executions from 301 cases. Typed parsing alone does not
  establish a behavioral regression for each candidate.
- Property/value coverage gaps remain visible in the generated report.

The four panic inputs were `hsl(calc(infinity) 100% 50%)`,
`hsl(calc(-infinity) 100% 50%)`, `hwb(calc(infinity) 20% 10%)`, and
`hwb(calc(-infinity) 20% 10%)`. Each returned the panic message
`assertion failed: (0.0..=1.0).contains(&hue)` in every mode. Chrome accepted
all four original inputs and serialized them to their WPT expectations.
The full run continued after each panic.

The 40 valid-input rejections comprise 32 `background-image` URL-modifier cases
and eight `width` cases with newer `var()` syntax. A separate specified-value
probe in Chrome matched WPT's expectations for 22 URL-modifier inputs. Chrome
also rejected the ten unknown URL-modifier cases and all eight newer `var()`
cases. These remain support candidates that need review; browser rejection
does not invalidate the pinned WPT expectation. URL probes did not load
resources or substitute WPT host templates.

## Starter browser run

All 488 mode executions completed:

- 456 `pass` executions.
- 14 `contextual-match` executions, which establish equivalence only in the
  derived fixture.
- Four `invalid-became-valid` executions for `column-gap: 10`, emitted as `10px`.
- Eight `computed-difference` executions from two animation shorthand cases.
- Four `baseline-failure` executions for one invalid-color case.
- Two `context-required` executions for lowered `light-dark()` output.

The animation inputs were
`anim paused both reverse, 4 1s -3s cubic-bezier(0, -2, 1, 3)` and the same
two animation items in reverse order. Printing replaces the unnamed animation
item with `none`, losing its duration, delay, timing function, and iteration
count in all four modes. The derived fixture exposes the changed longhands;
the intended optimization policy still needs review.

The baseline color input was `rgb(sign(0% - 0px), 0, 0)`. Chrome accepted it
before any Lightning CSS transformation, although the pinned WPT case expects
rejection. It is recorded as a browser baseline failure.

## CI changes and remaining scope

The workflow now runs on master pushes, PRs targeting master, and manual
dispatch. A separate harness test job includes synthetic browser tests. Corpus
diagnostics remain non-blocking and upload both HTML and JSON reports.
`actionlint`, `git diff --check`, and `cargo fmt --all -- --check` passed locally.

The previous WASM link failure is addressed by master's `--allow-undefined`
linker setting. Master's [test workflow](https://github.com/parcel-bundler/lightningcss/actions/runs/40225960544)
passed its Rust, JS, and WASM jobs. This local validation did not rebuild WASM.

Remaining work includes triaging production findings, validating the complete
browser corpus, executing original WPT fixtures, and adding the adapters listed
under [Next phases](README.md#next-phases). No findings were silently baselined.
