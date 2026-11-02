import {Worker} from "node:worker_threads";

// Keep one worker for the corpus, replacing it after an execution failure.
// Accept a page only after its Promise rejection checkpoint has completed.
export class CaptureWorker {
  constructor(timeout = 1500) {
    this.timeout = timeout;
  }
  start() {
    const worker = this.worker = new Worker(new URL("./capture-worker.mjs", import.meta.url), {
      // Test-runner/V8 flags are not all valid explicit Worker options.
      execArgv: process.execArgv.filter(arg => arg.startsWith("--unhandled-rejections=")),
    });
    worker.on("message", result => {
      if (worker === this.worker) this.finish(result);
    });
    worker.on("error", error => {
      if (worker === this.worker) this.fail(error.message);
    });
    worker.on("exit", code => {
      if (worker === this.worker) this.fail(`Capture worker exited ${code}`);
    });
  }
  finish(result) {
    if (!this.pending) return;
    const {resolve, timer} = this.pending;
    this.pending = null;
    clearTimeout(timer);
    resolve(result);
  }
  fail(reason) {
    const worker = this.worker;
    this.worker = null;
    void worker?.terminate();
    if (this.pending) {
      this.finish({file: this.pending.file, status: "unsupported", reason, cases: []});
    } else {
      this.error = Error(reason);
    }
  }
  request(file, document) {
    if (this.error) throw this.error;
    if (this.pending) throw Error("Concurrent capture requests are unsupported");
    if (!this.worker) this.start();
    return new Promise(resolve => {
      const timer = setTimeout(() => this.fail(`Page execution timed out after ${this.timeout}ms`), this.timeout);
      this.pending = {file, resolve, timer};
      try { this.worker.postMessage({file, document}); }
      catch (error) { this.fail(error.message); }
    });
  }
  async close() {
    const worker = this.worker;
    this.worker = null;
    this.finish({file: this.pending?.file, status: "unsupported", reason: "Capture worker closed", cases: []});
    await worker?.terminate();
    if (this.error) throw this.error;
  }
}
