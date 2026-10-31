import fs from "node:fs";
import path from "node:path";

export function sameFile(left, right) {
  if (path.resolve(left) === path.resolve(right)) return true;
  try {
    const a = fs.statSync(left, {bigint: true});
    const b = fs.statSync(right, {bigint: true});
    return a.dev === b.dev && a.ino === b.ino;
  } catch (error) {
    if (error.code === "ENOENT") return false;
    throw error;
  }
}
