import type {Targets} from './targets';

export interface TransformOptions {
  /** The filename being transformed. Used for error messages and source maps. */
  filename: string,
  /** The source code to transform. */
  code: Buffer,
  /** Whether to enable minification. */
  minify?: boolean,
  /** Whether to output a source map. */
  sourceMap?: boolean,
  /** The browser targets for the generated code. */
  targets?: Targets,
  /** Whether to enable various draft syntax. */
  drafts?: Drafts,
  /** Whether to compile this file as a CSS module. */
  cssModules?: boolean,
  /**
   * Whether to analyze dependencies (e.g. `@import` and `url()`).
   * When enabled, `@import` rules are removed, and `url()` dependencies
   * are replaced with hashed placeholders that can be replaced with the final
   * urls later (after bundling). Dependencies are returned as part of the result.
   */
  analyzeDependencies?: boolean
}

export interface Drafts {
  /** Whether to enable CSS nesting. */
  nesting?: boolean
}

export interface TransformResult {
  /** The transformed code. */
  code: Buffer,
  /** The generated source map, if enabled. */
  map: Buffer | void,
  /** CSS module exports, if enabled. */
  exports: CSSModuleExports | void,
  /** `@import` and `url()` dependencies, if enabled. */
  dependencies: Dependency[] | void
}

export type CSSModuleExports = {
  /** Maps exported (i.e. original) names to local names. */
  [name: string]: CSSModuleExport[]
};

export type CSSModuleExport = LocalCSSModuleExport | DependencyCSSModuleExport;

export interface LocalCSSModuleExport {
  type: 'local',
  /** The local (compiled) name for this export. */
  value: string
}

export interface DependencyCSSModuleExport {
  type: 'dependency',
  value: {
    /** The name to reference within the dependency. */
    name: string,
    /** The dependency specifier for the referenced file. */
    specifier: string
  }
}

export type Dependency = ImportDependency | UrlDependency;

export interface ImportDependency {
  type: 'import',
  /** The url of the `@import` dependency. */
  url: string,
  /** The media query for the `@import` rule. */
  media: string | null,
  /** The `supports()` query for the `@import` rule. */
  supports: string | null,
  /** The source location where the `@import` rule was found. */
  loc: SourceLocation
}

export interface UrlDependency {
  type: 'url',
  /** The url of the dependency. */
  url: string,
  /** The source location where the `url()` was found. */
  loc: SourceLocation
}

export interface SourceLocation {
  /** The start location of the dependency. */
  start: Location,
  /** The end location (inclusive) of the dependency. */
  end: Location
}

export interface Location {
  /** The line number (1-based). */
  line: number,
  /** The column number (0-based). */
  column: number
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
