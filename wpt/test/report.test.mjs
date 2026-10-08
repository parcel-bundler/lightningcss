import { test } from "node:test";
import assert from "node:assert/strict";
import { summarize, renderReport } from "../report.mjs";

const sample = {
  complete: false,
  harnessError: "test error",
  wptRevision: "a".repeat(40),
  results: [
    { id: "one", property: "color", value: "red", mode: "print", status: "recognized" },
    {
      id: "one",
      property: "color",
      value: "red",
      mode: "minify",
      status: "recognized",
      browser: { status: "computed-difference" },
    },
    {
      id: "two",
      property: "width",
      value: "new-value",
      mode: "print",
      status: "value-coverage-gap",
    },
    {
      id: "three",
      property: "width",
      value: "10px",
      mode: "print",
      status: "recognized",
      browser: { status: "baseline-failure" },
    },
  ],
  pages: [{ file: "css/example.html", status: "unsupported", reason: "needs adapter" }],
};

test("report separates executions from cases and keeps failure/coverage/baseline classifications", () => {
  const model = summarize(sample);
  assert.equal(model.executions, 4);
  assert.equal(model.properties[0].cases.length, 1);
  assert.equal(model.properties[0].cases[0].runs.length, 2);
  assert.deepEqual(model.totals, { failure: 1, coverage: 1, unresolved: 1, other: 1 });
  const recognized = model.classifications.find((c) => c.key === "parser:recognized");
  assert.equal(recognized.executions, 3);
  assert.equal(recognized.cases, 2);
  assert.equal(model.pages.length, 1);
  assert.ok(renderReport(sample).includes("Incomplete run"));
  assert.throws(() => summarize({}), /results array/);
});

test("embedded CSS, errors and titles cannot escape the HTML or JSON script", () => {
  const attack = '</script><img src=x onerror="window.injected=1">\u2028';
  const html = renderReport(
    { ...sample, results: [{ ...sample.results[0], value: attack, code: attack, error: attack }] },
    attack,
  );
  assert.ok(!html.includes(attack));
  assert.ok(!html.includes("<img"));
  const payload = html.match(/id="report-data">([\s\S]*?)<\/script>/)[1];
  assert.equal(JSON.parse(payload).properties[0].cases[0].value, attack);
});

test(
  "HTML report filters and lazily expands property/case details",
  { skip: !process.env.WPT_BROWSER_TESTS },
  async () => {
    const { default: puppeteer } = await import("puppeteer");
    const browser = await puppeteer.launch({
      headless: true,
      executablePath: process.env.WPT_CHROME,
    });
    try {
      const page = await browser.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(e.message));
      const attack = '</script><img src=x onerror="window.injected=1">';
      const results = Array.from({ length: 120 }, (_, i) => ({
        id: `case-${i}`,
        property: "color",
        value: i ? `bad-${i}` : attack,
        mode: "print",
        status: "invalid-typed",
        file: "css/test.html",
        code: "color:red",
      }));
      results.push({
        id: "valid",
        property: "width",
        value: "10px",
        mode: "print",
        status: "recognized",
      });
      await page.setContent(renderReport({ ...sample, results }));
      assert.equal(await page.$$eval(".property", (nodes) => nodes.length), 1);
      assert.equal(await page.$$eval(".case", (nodes) => nodes.length), 0);
      await page.click(".property > summary");
      await page.waitForSelector(".case");
      assert.equal(await page.$$eval(".case", (nodes) => nodes.length), 50);
      assert.equal(await page.$$eval(".case-body", (nodes) => nodes.length), 0);
      await page.click(".case > summary");
      await page.waitForSelector(".case-body");
      assert.equal(await page.$$eval(".case-body", (nodes) => nodes.length), 1);
      assert.equal(await page.evaluate(() => window.injected), undefined);
      assert.equal(await page.$$eval("img", (nodes) => nodes.length), 0);
      await page.click(".more");
      assert.equal(await page.$$eval(".case", (nodes) => nodes.length), 100);
      await page.select("#scope", "all");
      assert.equal(await page.$$eval(".property", (nodes) => nodes.length), 2);
      await page.select("#classification", "parser:recognized");
      assert.equal(
        await page.$eval(".property summary strong", (node) => node.textContent),
        "width",
      );
      await page.select("#classification", "");
      await page.type("#search", "does-not-exist");
      await page.waitForSelector(".empty");
      await page.click("#reset");
      assert.equal(await page.$$eval(".property", (nodes) => nodes.length), 1);
      assert.deepEqual(errors, []);
    } finally {
      await browser.close();
    }
  },
);
