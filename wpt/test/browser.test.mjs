import { test } from "node:test";
import assert from "node:assert/strict";
import { startBrowser } from "../browser.mjs";
import puppeteer from "puppeteer";

test(
  "browser oracle distinguishes regressions, equivalent spelling, and baseline failures",
  { skip: !process.env.WPT_BROWSER_TESTS },
  async () => {
    const browser = await startBrowser(process.env.WPT_CHROME);
    const valid = (property, value) => ({
      kind: "valid",
      property,
      value,
      expected: value,
      contextFree: true,
    });
    try {
      assert.equal((await browser.check(valid("width", "10px"), "width: 10px")).status, "pass");
      assert.equal(
        (await browser.check(valid("width", "10px"), "width: 11px")).status,
        "computed-difference",
      );
      assert.equal(
        (await browser.check(valid("color", "red"), "color: #f00")).status,
        "contextual-match",
      );
      assert.equal(
        (await browser.check(valid("width", "bogus"), "width: 10px")).status,
        "baseline-failure",
      );
      assert.equal(
        (await browser.check({ ...valid("width", "bogus"), kind: "invalid" }, "width: 10px"))
          .status,
        "invalid-became-valid",
      );
      assert.equal(
        (await browser.check({ ...valid("width", "bogus"), kind: "invalid" }, "width: bogus"))
          .status,
        "pass",
      );
      assert.equal(
        (await browser.check({ ...valid("width", "10px"), contextFree: false }, "width: 10px"))
          .status,
        "fixture-required",
      );
      const shorthand = {
        kind: "shorthand",
        property: "margin",
        value: "1px",
        contextFree: true,
        expected: {
          "margin-top": "1px",
          "margin-right": "1px",
          "margin-bottom": "1px",
          "margin-left": "1px",
        },
      };
      assert.equal((await browser.check(shorthand, "margin: 1px")).status, "pass");
      assert.equal(
        (await browser.check({ ...shorthand, expected: { "margin-top": "1px" } }, "margin: 1px"))
          .status,
        "baseline-failure",
      );
      assert.equal((await browser.check(shorthand, "margin: 2px")).status, "computed-difference");
      assert.equal(
        (await browser.check(shorthand, "margin-top: 1px; margin-right: 1px; margin-bottom: 1px"))
          .status,
        "shorthand-reset-changed",
      );
    } finally {
      await browser.close();
    }
  },
);

test("browser startup failures close the launched process", async () => {
  const launch = puppeteer.launch;
  try {
    for (const phase of ["newPage", "version"]) {
      let closed = false;
      puppeteer.launch = async () => ({
        newPage: async () => {
          if (phase === "newPage") throw Error("newPage failed");
          return { setContent: async () => {} };
        },
        version: async () => { throw Error("version failed"); },
        close: async () => { closed = true; },
      });
      await assert.rejects(startBrowser(), new RegExp(`${phase} failed`));
      assert.ok(closed);
    }
  } finally { puppeteer.launch = launch; }
});

test("a stalled browser evaluation has a deadline", async () => {
  const launch = puppeteer.launch;
  let closed = false;
  let browser;
  try {
    puppeteer.launch = async () => ({
      newPage: async () => ({ setContent: async () => {}, evaluate: () => new Promise(() => {}) }),
      version: async () => "test", process: () => undefined,
      close: async () => { closed = true; },
    });
    browser = await startBrowser(undefined, 25);
    await assert.rejects(browser.check({ contextFree: true, value: "red" }, "color:red"), /timed out/);
  } finally { await browser?.close(); puppeteer.launch = launch; }
  assert.ok(closed);
});

test(
  "valid-value helpers detect lost resets, injected resets and changed importance",
  { skip: !process.env.WPT_BROWSER_TESTS },
  async () => {
    const browser = await startBrowser(process.env.WPT_CHROME);
    const valid = (property, value) => ({ kind: "valid", property, value, expected: value, contextFree: true });
    try {
      for (const value of ["0px", "inherit", "initial"])
        assert.equal((await browser.check(valid("margin", value), `margin-top:${value}`)).status,
          "shorthand-reset-changed");
      assert.equal((await browser.check(valid("width", "10px"), "width:10px;color:initial")).status,
        "cascade-changed");
      assert.equal((await browser.check(valid("color", "red"), "color:red!important")).status,
        "priority-changed");
      assert.equal((await browser.check(valid("width", "unset"), "width:initial")).status,
        "contextual-match");
      assert.equal((await browser.check(valid("width", "10px"),
        "--lightningcss-light: initial; --lightningcss-dark: ; width:10px")).status,
        "context-required");
    } finally { await browser.close(); }
  },
);

test(
  "empty invalid values preserve a preceding CSS declaration",
  { skip: !process.env.WPT_BROWSER_TESTS },
  async () => {
    const browser = await startBrowser(process.env.WPT_CHROME);
    try {
      const invalid = { kind: "invalid", property: "color", value: "", contextFree: true };
      assert.equal((await browser.check(invalid, "color:")).status, "pass");
      assert.equal((await browser.check(invalid, "")).status, "pass");
      assert.equal((await browser.check(invalid, "color:red")).status, "invalid-became-valid");
    } finally { await browser.close(); }
  },
);

test(
  "step easing aliases match without hiding different step counts or positions",
  { skip: !process.env.WPT_BROWSER_TESTS },
  async () => {
    const browser = await startBrowser(process.env.WPT_CHROME);
    const test = { kind: "valid", property: "animation-timing-function",
      value: "steps(2, jump-start)", expected: "steps(2, jump-start)", contextFree: true };
    try {
      assert.equal((await browser.check(test, "animation-timing-function:steps(2,start)")).status,
        "contextual-match");
      assert.equal((await browser.check(test, "animation-timing-function:steps(3,start)")).status,
        "computed-difference");
      assert.equal((await browser.check(test, "animation-timing-function:steps(2,end)")).status,
        "computed-difference");
      const end = { ...test, value: "steps(2, jump-end)",
        expected: ["steps(2, jump-end)", "steps(2, end)", "steps(2)"] };
      assert.ok(["pass", "contextual-match"].includes(
        (await browser.check(end, "animation-timing-function:steps(2)")).status));
      const named = { ...test, property: "animation", value: "1s steps(2, jump-start) anim",
        expected: ["1s steps(2, jump-start) anim", "1s steps(2, jump-start) 0s 1 normal none running anim"] };
      const normalized = await browser.check(named, "animation:1s steps(2,start) anim");
      assert.equal(normalized.status, "contextual-match");
      assert.ok(normalized.computed.every((item) => item.property !== "animation"));
    } finally { await browser.close(); }
  },
);

test(
  "escaped URL functions remain outside the context-free browser lane",
  { skip: !process.env.WPT_BROWSER_TESTS },
  async () => {
    const browser = await startBrowser(process.env.WPT_CHROME);
    try {
      const result = await browser.check({ kind: "valid", property: "background-image",
        value: String.raw`u\72l("relative.png")`, expected: 'url("relative.png")', contextFree: true },
        'background-image:url("relative.png")');
      assert.equal(result.status, "context-required");
    } finally { await browser.close(); }
  },
);

test(
  "browser oracle accepts serialization alternatives and rejects incorrect longhand expectations",
  { skip: !process.env.WPT_BROWSER_TESTS },
  async () => {
    const browser = await startBrowser(process.env.WPT_CHROME);
    try {
      const valid = {
        kind: "valid",
        property: "color",
        value: "red",
        expected: ["red", "rgb(255, 0, 0)"],
        contextFree: true,
      };
      assert.equal((await browser.check(valid, "color: red")).status, "pass");
      assert.equal(
        (await browser.check({ ...valid, expected: ["blue", "green"] }, "color: red")).status,
        "baseline-failure",
      );
      const shorthand = {
        kind: "shorthand",
        property: "margin",
        value: "1px",
        expected: {
          "margin-top": "2px",
          "margin-right": "1px",
          "margin-bottom": "1px",
          "margin-left": "1px",
        },
        contextFree: true,
      };
      const result = await browser.check(shorthand, "margin: 1px");
      assert.equal(result.status, "baseline-failure");
      assert.deepEqual(result.differences, [{ property: "margin-top", expected: "2px", actual: "1px" }]);
    } finally {
      await browser.close();
    }
  },
);

test(
  "escaped substitutions and attribute dependencies require context on either side",
  {skip: !process.env.WPT_BROWSER_TESTS},
  async () => {
    const browser = await startBrowser(process.env.WPT_CHROME);
    const valid = (property, value) => ({kind: "valid", property, value, expected: value, contextFree: true});
    try {
      // The environment inset is zero in this fixture; matching 0px cannot
      // establish that replacing the environment reference preserves behavior.
      assert.equal((await browser.check(valid("width", String.raw`e\6ev(safe-area-inset-top)`),
        "width:0px")).status, "context-required");
      assert.equal((await browser.check(valid("color", String.raw`v\61 r(--a)`),
        String.raw`color:v\61 r(--b)`)).status, "context-required");
      assert.equal((await browser.check(valid("color", "red"),
        String.raw`color:v\61 r(--b)`)).status, "context-required");
      assert.equal((await browser.check(valid("width", "0px"),
        String.raw`width:e\6ev(safe-area-inset-top)`)).status, "context-required");
      assert.equal((await browser.check(valid("width", "attr(data-width px, 0px)"),
        "width:0px")).status, "context-required");
      assert.equal((await browser.check(valid("--plain", "value"), "--plain:value")).status, "pass");
      assert.equal((await browser.check(valid("width", "10px"), "width:11px")).status,
        "computed-difference");
    } finally { await browser.close(); }
  },
);
