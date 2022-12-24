import type { Angle, CssColor, Rule, CustomProperty, EnvironmentVariable, Function, Image, LengthValue, MediaQuery, Declaration, Ratio, Resolution, Selector, SupportsCondition, Time, Token, TokenOrValue, UnknownAtRule, Url, Variable } from './ast';
import type { Targets } from './targets';

export * from './ast';

export interface TransformOptions {
  /** The filename being transformed. Used for error messages and source maps. */
  filename: string,
  /** The source code to transform. */
  code: Buffer,
  /** Whether to enable minification. */
  minify?: boolean,
  /** Whether to output a source map. */
  sourceMap?: boolean,
  /** An input source map to extend. */
  inputSourceMap?: string,
  /** 
   * An optional project root path, used as the source root in the output source map.
   * Also used to generate relative paths for sources used in CSS module hashes.
   */
  projectRoot?: string,
  /** The browser targets for the generated code. */
  targets?: Targets,
  /** Whether to enable various draft syntax. */
  drafts?: Drafts,
  /** Whether to compile this file as a CSS module. */
  cssModules?: boolean | CSSModulesConfig,
  /**
   * Whether to analyze dependencies (e.g. `@import` and `url()`).
   * When enabled, `@import` rules are removed, and `url()` dependencies
   * are replaced with hashed placeholders that can be replaced with the final
   * urls later (after bundling). Dependencies are returned as part of the result.
   */
  analyzeDependencies?: boolean | DependencyOptions,
  /** 
   * Replaces user action pseudo classes with class names that can be applied from JavaScript.
   * This is useful for polyfills, for example.
   */
  pseudoClasses?: PseudoClasses,
  /**
   * A list of class names, ids, and custom identifiers (e.g. @keyframes) that are known
   * to be unused. These will be removed during minification. Note that these are not
   * selectors but individual names (without any . or # prefixes).
   */
  unusedSymbols?: string[],
  /**
   * Whether to ignore invalid rules and declarations rather than erroring.
   * When enabled, warnings are returned, and the invalid rule or declaration is
   * omitted from the output code.
   */
  errorRecovery?: boolean,
  /**
   * An AST visitor object. This allows custom transforms or analysis to be implemented in JavaScript.
   * Multiple visitors can be composed into one using the `composeVisitors` function.
   * For optimal performance, visitors should be as specific as possible about what types of values
   * they care about so that JavaScript has to be called as little as possible.
   */
  visitor?: Visitor
}

type FindByType<Union, Name> = Union extends { type: Name } ? Union : never;
type RuleVisitor<R = Rule> = ((rule: R) => Rule | Rule[] | void);
type MappedRuleVisitors = {
  [Name in Exclude<Rule['type'], 'unknown' | 'custom'>]?: RuleVisitor<FindByType<Rule, Name>>;
}

type UnknownVisitors = {
  [name: string]: RuleVisitor<UnknownAtRule>
}

type RuleVisitors = MappedRuleVisitors & {
  unknown?: UnknownVisitors | RuleVisitor<UnknownAtRule>
};

type FindProperty<Union, Name> = Union extends { property: Name } ? Union : never;
type DeclarationVisitor<P = Declaration> = ((property: P) => Declaration | Declaration[] | void);
type MappedDeclarationVisitors = {
  [Name in Exclude<Declaration['property'], 'unparsed' | 'custom'>]?: DeclarationVisitor<FindProperty<Declaration, Name> | FindProperty<Declaration, 'unparsed'>>;
}

type CustomPropertyVisitors = {
  [name: string]: DeclarationVisitor<CustomProperty>
}

type DeclarationVisitors = MappedDeclarationVisitors & {
  custom?: CustomPropertyVisitors | DeclarationVisitor<CustomProperty>
}

type TokenVisitor = (token: Token) => TokenOrValue | TokenOrValue[] | void;
type VisitableTokenTypes = 'ident' | 'at-keyword' | 'hash' | 'id-hash' | 'string' | 'number' | 'percentage' | 'dimension';
type TokenVisitors = {
  [Name in VisitableTokenTypes]?: (token: FindByType<Token, Name>) => TokenOrValue | TokenOrValue[] | void;
}

type FunctionVisitor = (fn: Function) => TokenOrValue | TokenOrValue[] | void;
type EnvironmentVariableVisitor = (env: EnvironmentVariable) => TokenOrValue | TokenOrValue[] | void;
type EnvironmentVariableVisitors = {
  [name: string]: EnvironmentVariableVisitor
};

export interface Visitor {
  Rule?: RuleVisitor | RuleVisitors;
  RuleExit?: RuleVisitor | RuleVisitors;
  Declaration?: DeclarationVisitor | DeclarationVisitors;
  DeclarationExit?: DeclarationVisitor | DeclarationVisitors;
  Url?(url: Url): Url | void;
  Color?(color: CssColor): CssColor | void;
  Image?(image: Image): Image | void;
  ImageExit?(image: Image): Image | void;
  Length?(length: LengthValue): LengthValue | void;
  Angle?(angle: Angle): Angle | void;
  Ratio?(ratio: Ratio): Ratio | void;
  Resolution?(resolution: Resolution): Resolution | void;
  Time?(time: Time): Time | void;
  CustomIdent?(ident: string): string | void;
  DashedIdent?(ident: string): string | void;
  MediaQuery?(query: MediaQuery): MediaQuery | MediaQuery[] | void;
  MediaQueryExit?(query: MediaQuery): MediaQuery | MediaQuery[] | void;
  SupportsCondition?(condition: SupportsCondition): SupportsCondition;
  SupportsConditionExit?(condition: SupportsCondition): SupportsCondition;
  Selector?(selector: Selector): Selector | Selector[] | void;
  Token?: TokenVisitor | TokenVisitors;
  Function?: FunctionVisitor | { [name: string]: FunctionVisitor };
  FunctionExit?: FunctionVisitor | { [name: string]: FunctionVisitor };
  Variable?(variable: Variable): TokenOrValue | TokenOrValue[] | void;
  VariableExit?(variable: Variable): TokenOrValue | TokenOrValue[] | void;
  EnvironmentVariable?: EnvironmentVariableVisitor | EnvironmentVariableVisitors;
  EnvironmentVariableExit?: EnvironmentVariableVisitor | EnvironmentVariableVisitors;
}

export interface DependencyOptions {
  /** Whether to preserve `@import` rules rather than removing them. */
  preserveImports?: boolean
}

export type BundleOptions = Omit<TransformOptions, 'code'>;

export interface BundleAsyncOptions extends BundleOptions {
  resolver?: Resolver;
}

/** Custom resolver to use when loading CSS files. */
export interface Resolver {
  /** Read the given file and return its contents as a string. */
  read?: (file: string) => string | Promise<string>;

  /**
   * Resolve the given CSS import specifier from the provided originating file to a
   * path which gets passed to `read()`.
   */
  resolve?: (specifier: string, originatingFile: string) => string | Promise<string>;
}

export interface Drafts {
  /** Whether to enable CSS nesting. */
  nesting?: boolean,
  /** Whether to enable @custom-media rules. */
  customMedia?: boolean
}

export interface PseudoClasses {
  hover?: string,
  active?: string,
  focus?: string,
  focusVisible?: string,
  focusWithin?: string
}

export interface TransformResult {
  /** The transformed code. */
  code: Buffer,
  /** The generated source map, if enabled. */
  map: Buffer | void,
  /** CSS module exports, if enabled. */
  exports: CSSModuleExports | void,
  /** CSS module references, if `dashedIdents` is enabled. */
  references: CSSModuleReferences,
  /** `@import` and `url()` dependencies, if enabled. */
  dependencies: Dependency[] | void,
  /** Warnings that occurred during compilation. */
  warnings: Warning[]
}

export interface Warning {
  message: string,
  type: string,
  value?: any,
  loc: ErrorLocation
}

export interface CSSModulesConfig {
  /** The pattern to use when renaming class names and other identifiers. Default is `[hash]_[local]`. */
  pattern: string,
  /** Whether to rename dashed identifiers, e.g. custom properties. */
  dashedIdents: boolean
}

export type CSSModuleExports = {
  /** Maps exported (i.e. original) names to local names. */
  [name: string]: CSSModuleExport
};

export interface CSSModuleExport {
  /** The local (compiled) name for this export. */
  name: string,
  /** Whether the export is referenced in this file. */
  isReferenced: boolean,
  /** Other names that are composed by this export. */
  composes: CSSModuleReference[]
}

export type CSSModuleReferences = {
  /** Maps placeholder names to references. */
  [name: string]: DependencyCSSModuleReference,
};

export type CSSModuleReference = LocalCSSModuleReference | GlobalCSSModuleReference | DependencyCSSModuleReference;

export interface LocalCSSModuleReference {
  type: 'local',
  /** The local (compiled) name for the reference. */
  name: string,
}

export interface GlobalCSSModuleReference {
  type: 'global',
  /** The referenced global name. */
  name: string,
}

export interface DependencyCSSModuleReference {
  type: 'dependency',
  /** The name to reference within the dependency. */
  name: string,
  /** The dependency specifier for the referenced file. */
  specifier: string
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
  loc: SourceLocation,
  /** The placeholder that the url was replaced with. */
  placeholder: string
}

export interface SourceLocation {
  /** The file path in which the dependency exists. */
  filePath: string,
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

export interface ErrorLocation extends Location {
  filename: string
}

/**
 * Compiles a CSS file, including optionally minifying and lowering syntax to the given
 * targets. A source map may also be generated, but this is not enabled by default.
 */
export declare function transform(options: TransformOptions): TransformResult;

export interface TransformAttributeOptions {
  /** The filename in which the style attribute appeared. Used for error messages and dependencies. */
  filename?: string,
  /** The source code to transform. */
  code: Buffer,
  /** Whether to enable minification. */
  minify?: boolean,
  /** The browser targets for the generated code. */
  targets?: Targets,
  /**
   * Whether to analyze `url()` dependencies.
   * When enabled, `url()` dependencies are replaced with hashed placeholders 
   * that can be replaced with the final urls later (after bundling).
   * Dependencies are returned as part of the result.
   */
  analyzeDependencies?: boolean,
  /**
   * Whether to ignore invalid rules and declarations rather than erroring.
   * When enabled, warnings are returned, and the invalid rule or declaration is
   * omitted from the output code.
   */
  errorRecovery?: boolean,
  /**
   * An AST visitor object. This allows custom transforms or analysis to be implemented in JavaScript.
   * Multiple visitors can be composed into one using the `composeVisitors` function.
   * For optimal performance, visitors should be as specific as possible about what types of values
   * they care about so that JavaScript has to be called as little as possible.
   */
  visitor?: Visitor
}

export interface TransformAttributeResult {
  /** The transformed code. */
  code: Buffer,
  /** `@import` and `url()` dependencies, if enabled. */
  dependencies: Dependency[] | void,
  /** Warnings that occurred during compilation. */
  warnings: Warning[]
}

/**
 * Compiles a single CSS declaration list, such as an inline style attribute in HTML.
 */
export declare function transformStyleAttribute(options: TransformAttributeOptions): TransformAttributeResult;

/**
 * Converts a browserslist result into targets that can be passed to lightningcss.
 * @param browserslist the result of calling `browserslist`
 */
export declare function browserslistToTargets(browserslist: string[]): Targets;

/**
 * Bundles a CSS file and its dependencies, inlining @import rules.
 */
export declare function bundle(options: BundleOptions): TransformResult;

/**
 * Bundles a CSS file and its dependencies asynchronously, inlining @import rules.
 */
export declare function bundleAsync(options: BundleAsyncOptions): Promise<TransformResult>;

/**
 * Composes multiple visitor objects into a single one.
 */
export declare function composeVisitors(visitors: Visitor[]): Visitor;
