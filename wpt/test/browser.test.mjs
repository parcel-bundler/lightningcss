import { test } from "node:test";
import assert from "node:assert/strict";
import { startBrowser } from "../browser.mjs";

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
