import type {Targets} from './targets';

export interface TransformOptions {
  /** The filename being transformed. Used for error messages and source maps. */
  filename: string,
  /** The source code to transform. */
  code: Buffer,
  /** Whether to enable minification. */
  minify?: boolean,
  /** Whether to output a source map. */
  source_map?: boolean,
  /** The browser targets for the generated code. */
  targets?: Targets
}

export interface TransformResult {
  /** The transformed code. */
  code: Buffer,
  /** The generated source map, if enabled. */
  map: Buffer | void
}

/**
 * Compiles a CSS file, including optionally minifying and lowering syntax to the given
 * targets. A source map may also be generated, but this is not enabled by default.
 */
export declare function transform(options: TransformOptions): TransformResult;

export interface TransformAttributeOptions {
  /** The source code to transform. */
  code: Buffer,
  /** Whether to enable minification. */
  minify?: boolean,
  /** The browser targets for the generated code. */
  targets?: Targets
}

/**
 * Compiles a single CSS declaration list, such as an inline style attribute in HTML.
 */
export declare function transformStyleAttribute(options: TransformAttributeOptions): Buffer;
