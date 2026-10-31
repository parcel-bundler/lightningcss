import {parentPort} from "node:worker_threads";
import {setImmediate} from "node:timers/promises";
import {capture, errorMessage} from "./extract.mjs";

let rejection;
process.on("unhandledRejection", error => { rejection ??= errorMessage(error); });
parentPort.on("message", async ({file, document}) => {
  rejection = undefined;
  const result = capture(file, document);
  // VM Promise rejections are reported after the synchronous evaluation returns.
  await setImmediate();
  parentPort.postMessage(rejection === undefined ? result : {
    file, status: "unsupported", reason: rejection, cases: [],
  });
});
