import { test } from "node:test";
import assert from "node:assert/strict";
import { Adapter } from "../adapter.mjs";
import { runCases } from "../run.mjs";
import { exitCode } from "../baseline.mjs";

test("late responses cannot be attributed to the next case, and recovery restarts the child", async () => {
  const script = `let previous;
    require('readline').createInterface({input:process.stdin}).on('line',line=>{
      const v=JSON.parse(line);
      if(v.first){previous=v.requestId;console.log(JSON.stringify({requestId:v.requestId,recognition:'typed',code:'color:red'}));}
      else {console.log(JSON.stringify({requestId:previous,recognition:'typed',code:'color:red'}));
        setTimeout(()=>console.log(JSON.stringify({requestId:v.requestId,recognition:'typed',code:'color:blue'})),100);}
    });`;
  const adapter = new Adapter(process.execPath, 1000, ["-e", script]);
  try {
    assert.equal((await adapter.request({ first: true })).code, "color:red");
    assert.equal((await adapter.request({ first: false })).recognition, "protocol-error");
    assert.equal((await adapter.request({ first: true })).code, "color:red");
  } finally { adapter.close(); }
});

test("malformed response shapes are isolated as protocol errors", async () => {
  const script = `require('readline').createInterface({input:process.stdin}).on('line',line=>{
    const v=JSON.parse(line);
    if(!Object.hasOwn(v,'bad'))return console.log(JSON.stringify({requestId:v.requestId,recognition:'typed',code:'color:red'}));
    if(v.bad==='malformed')return console.log('not JSON');
    console.log(JSON.stringify(v.bad && !Array.isArray(v.bad) ? {...v.bad,requestId:v.requestId}:v.bad));
  });`;
  const adapter = new Adapter(process.execPath, 1000, ["-e", script]);
  try {
    for (const bad of [null, [], {}, { recognition: "typed" }, { recognition: "typed", code: 42 },
      { recognition: "error" }, { recognition: "unknown", code: "color:red" },
      { recognition: "typed", code: "color:red", id: "wrong-case" },
      { recognition: "typed", code: "color:red", error: "unexpected error" }, "malformed"]) {
      assert.equal((await adapter.request({ bad })).recognition, "protocol-error");
      assert.equal((await adapter.request({})).code, "color:red");
    }
  } finally { adapter.close(); }
});

test("closing an adapter settles an outstanding request", async () => {
  const adapter = new Adapter(process.execPath, 1000, ["-e", "setInterval(()=>{},1000)"]);
  const pending = adapter.request({});
  adapter.close();
  assert.equal((await pending).recognition, "protocol-error");
  assert.equal(adapter.pending, null);
});

test("serialization failures leave no pending request or child process", () => {
  const adapter = new Adapter(process.execPath);
  const circular = {};
  circular.self = circular;
  assert.throws(() => adapter.request(circular), /circular/i);
  assert.ok(!adapter.pending);
  assert.ok(!adapter.child);
});

const duplicateResponseScript = `
  require('readline').createInterface({input:process.stdin}).on('line',line=>{
    const v=JSON.parse(line);
    const response=JSON.stringify({requestId:v.requestId,recognition:'typed',code:'color:red'});
    process.stdout.write(response+'\\n'+response+'\\n');
  });`;

test("duplicate responses between requests cannot be silently discarded", async () => {
  const adapter = new Adapter(process.execPath, 1000, ["-e", duplicateResponseScript]);
  try {
    assert.equal((await adapter.request({})).code, "color:red");
    // Both lines are emitted in one write; readline delivers the idle duplicate
    // before the promise continuation requests the next case.
    assert.throws(() => adapter.request({}), /Unsolicited adapter response/);
    assert.equal(adapter.child, null);
  } finally { adapter.close(); }
});

test("cleanup reports an unsolicited response after the final case", async () => {
  const adapter = new Adapter(process.execPath, 1000, ["-e", duplicateResponseScript]);
  assert.equal((await adapter.request({})).code, "color:red");
  assert.throws(() => adapter.close(), /Unsolicited adapter response/);
  assert.equal(adapter.child, null);
  // Cleanup remains safe to repeat after reporting the error.
  adapter.close();
});

test("a final duplicate response makes the run fail with retained parser results", async () => {
  const adapter = new Adapter(process.execPath, 1000, ["-e", duplicateResponseScript]);
  const report = {modes: ["print"], results: [], pages: []};
  await runCases(report, [{id: "red", kind: "valid", property: "color", value: "red"}], adapter);
  report.complete = !report.harnessError && report.results.length === 1;
  assert.equal(report.results[0].code, "color:red");
  assert.match(report.harnessError, /Unsolicited adapter response/);
  assert.equal(report.complete, false);
  assert.equal(exitCode(report), 2);
});

test("idle adapter exits fail both the next request and final cleanup", async () => {
  const script = `require('readline').createInterface({input:process.stdin}).on('line',line=>{
    const v=JSON.parse(line);
    console.log(JSON.stringify({requestId:v.requestId,recognition:'typed',code:'color:red'}));
    setTimeout(()=>process.exit(17),10);
  });`;
  for (const action of ['request', 'close']) {
    const adapter = new Adapter(process.execPath, 1000, ['-e',script]);
    try {
      assert.equal((await adapter.request({})).code, 'color:red');
      await new Promise(resolve => adapter.child.once('exit', resolve));
      assert.throws(() => adapter[action]({}), /Unexpected adapter exit 17/);
    } finally { adapter.close(); }
  }
});

test("an idle crash during browser checks retains results and fails the run", async () => {
  const script = `require('readline').createInterface({input:process.stdin}).on('line',line=>{
    const v=JSON.parse(line);
    console.log(JSON.stringify({requestId:v.requestId,recognition:'typed',code:'color:red'}));
    setTimeout(()=>process.exit(17),10);
  });`;
  const adapter = new Adapter(process.execPath, 1000, ['-e',script]);
  const report = {modes:['print'], results:[], pages:[]};
  await runCases(report, [{id:'red',kind:'valid',property:'color',value:'red'}], adapter, async () => ({
    check: async () => {
      await new Promise(resolve => adapter.child.once('exit', resolve));
      return {status:'pass'};
    }, close: async () => {},
  }));
  report.complete = !report.harnessError && report.results.length === 1;
  assert.match(report.harnessError, /Unexpected adapter exit 17/);
  assert.equal(report.results[0].browser.status, 'pass');
  assert.equal(exitCode(report), 2);
});
