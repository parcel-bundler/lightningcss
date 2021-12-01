import type {Targets} from './targets';

export interface TransformOptions {
  filename: string,
  code: Buffer,
  minify: boolean,
  source_map: boolean,
  targets: Targets
}

export interface TransformResult {
  code: Buffer,
  map: Buffer
}

export declare function transform(options: TransformOptions): TransformResult;
