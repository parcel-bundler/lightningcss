import { test } from "node:test";
import assert from "node:assert/strict";
import { Adapter } from "../adapter.mjs";

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
