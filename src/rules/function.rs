//! The `@function` rule.

use super::{CssRule, CssRuleList, Location};
use crate::declaration::DeclarationBlock;
use crate::error::{ErrorWithLocation, MinifyError, MinifyErrorKind, ParserError, PrinterError};
use crate::printer::Printer;
use crate::properties::custom::{CustomPropertyName, Function, Token, TokenList, TokenOrValue};
use crate::properties::Property;
use crate::stylesheet::{ParserOptions, PrinterOptions};
use crate::traits::{Parse, ParseWithOptions, ToCss};
use crate::values::ident::DashedIdent;
use crate::values::length::Length;
use crate::values::syntax::{ParsedComponent, SyntaxString};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
#[cfg(feature = "into_owned")]
use static_self::IntoOwned;
use std::collections::{HashMap, HashSet};

/// A [@function](https://drafts.csswg.org/css-mixins-1/#function-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct FunctionRule<'i> {
  /// The name of the function.
  #[cfg_attr(feature = "visitor", skip_visit)]
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: DashedIdent<'i>,
  /// The parameters of the function.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub parameters: Vec<FunctionParameter<'i>>,
  /// The syntax of the value the function returns, if declared.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub returns: Option<SyntaxString>,
  /// The value of the `result` descriptor.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub result: Option<TokenList<'i>>,
  /// Local custom properties declared in the body of the function.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub locals: Vec<FunctionLocal<'i>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

/// A parameter of a [@function](https://drafts.csswg.org/css-mixins-1/#function-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct FunctionParameter<'i> {
  /// The name of the parameter.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: DashedIdent<'i>,
  /// The syntax of the parameter, if declared.
  pub ty: Option<SyntaxString>,
  /// The value used when no argument is passed for the parameter.
  pub default: Option<TokenList<'i>>,
}

/// A local custom property declared in the body of a [@function](https://drafts.csswg.org/css-mixins-1/#function-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct FunctionLocal<'i> {
  /// The name of the custom property.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: DashedIdent<'i>,
  /// The value of the custom property.
  pub value: TokenList<'i>,
}

impl<'i> FunctionRule<'i> {
  pub(crate) fn parse_prelude<'t>(
    input: &mut Parser<'i, 't>,
  ) -> Result<(DashedIdent<'i>, Vec<FunctionParameter<'i>>, Option<SyntaxString>), ParseError<'i, ParserError<'i>>>
  {
    let name = input.expect_function()?.clone();
    if !name.starts_with("--") {
      return Err(input.new_custom_error(ParserError::AtRulePreludeInvalid));
    }
    let name = DashedIdent(name.into());
    let parameters = input.parse_nested_block(|input| {
      if input.is_exhausted() {
        return Ok(Vec::new());
      }
      input.parse_comma_separated(FunctionParameter::parse)
    })?;

    let returns = if input.try_parse(|input| input.expect_ident_matching("returns")).is_ok() {
      let start = input.position();
      while input.next().is_ok() {}
      let syntax = input.slice_from(start);
      Some(
        SyntaxString::parse_string(syntax)
          .map_err(|_| input.new_custom_error(ParserError::AtRulePreludeInvalid))?,
      )
    } else {
      None
    };

    Ok((name, parameters, returns))
  }

  pub(crate) fn parse_body<'t>(
    name: DashedIdent<'i>,
    parameters: Vec<FunctionParameter<'i>>,
    returns: Option<SyntaxString>,
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'i>,
    loc: Location,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut parser = FunctionBodyParser {
      options,
      result: None,
      locals: Vec::new(),
    };

    let mut decl_parser = RuleBodyParser::new(input, &mut parser);
    while let Some(decl) = decl_parser.next() {
      if let Err((e, _)) = decl {
        return Err(e);
      }
    }

    Ok(FunctionRule {
      name,
      parameters,
      returns,
      result: parser.result,
      locals: parser.locals,
      loc,
    })
  }
}

impl<'i> FunctionParameter<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = DashedIdent::parse(input)?;

    let start = input.position();
    let mut ty = None;
    let mut default = None;
    loop {
      let is_colon = input.next().map(|token| matches!(token, cssparser::Token::Colon));
      match is_colon {
        Ok(true) => {
          ty = Some(input.slice_from(start).trim_end_matches(':'));
          let default_start = input.position();
          while input.next().is_ok() {}
          default = Some(input.slice_from(default_start));
          break;
        }
        Ok(false) => {}
        Err(_) => break,
      }
    }
    let ty = ty.unwrap_or_else(|| input.slice_from(start));

    let ty = if ty.trim().is_empty() {
      None
    } else {
      Some(SyntaxString::parse_string(ty).map_err(|_| input.new_custom_error(ParserError::AtRulePreludeInvalid))?)
    };

    let default = match default {
      Some(default) if !default.trim().is_empty() => Some(
        TokenList::parse_string_with_options(default.trim(), ParserOptions::default())
          .map_err(|_| input.new_custom_error(ParserError::AtRulePreludeInvalid))?,
      ),
      _ => None,
    };

    if let (Some(ty), Some(default)) = (&ty, &default) {
      if matches!(classify(default, ty), TypeMatch::NoMatch) {
        return Err(input.new_custom_error(ParserError::InvalidValue));
      }
    }

    Ok(FunctionParameter { name, ty, default })
  }
}

struct FunctionBodyParser<'a, 'i> {
  options: &'a ParserOptions<'i>,
  result: Option<TokenList<'i>>,
  locals: Vec<FunctionLocal<'i>>,
}

impl<'a, 'i> cssparser::DeclarationParser<'i> for FunctionBodyParser<'a, 'i> {
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
    _declaration_start: &ParserState,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    if name.eq_ignore_ascii_case("result") {
      self.result = Some(TokenList::parse(input, self.options, 0)?);
    } else if name.starts_with("--") {
      let value = TokenList::parse(input, self.options, 0)?;
      self.locals.push(FunctionLocal {
        name: DashedIdent(name.into()),
        value,
      });
    } else {
      return Err(input.new_custom_error(ParserError::InvalidDeclaration));
    }
    Ok(())
  }
}

/// Default methods reject all at rules.
impl<'a, 'i> AtRuleParser<'i> for FunctionBodyParser<'a, 'i> {
  type Prelude = ();
  type AtRule = ();
  type Error = ParserError<'i>;
}

impl<'a, 'i> QualifiedRuleParser<'i> for FunctionBodyParser<'a, 'i> {
  type Prelude = ();
  type QualifiedRule = ();
  type Error = ParserError<'i>;
}

impl<'a, 'i> RuleBodyItemParser<'i, (), ParserError<'i>> for FunctionBodyParser<'a, 'i> {
  fn parse_qualified(&self) -> bool {
    false
  }

  fn parse_declarations(&self) -> bool {
    true
  }
}

impl<'i> ToCss for FunctionRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@function ")?;
    self.name.to_css(dest)?;
    dest.write_char('(')?;
    let mut first = true;
    for param in &self.parameters {
      if first {
        first = false;
      } else {
        dest.delim(',', false)?;
      }
      param.name.to_css(dest)?;
      if let Some(ty) = &param.ty {
        dest.write_char(' ')?;
        write_syntax(ty, dest)?;
      }
      if let Some(default) = &param.default {
        dest.write_char(':')?;
        dest.whitespace()?;
        default.to_css(dest, false)?;
      }
    }
    dest.write_char(')')?;

    if let Some(returns) = &self.returns {
      dest.write_str(" returns ")?;
      write_syntax(returns, dest)?;
    }

    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    for local in &self.locals {
      dest.newline()?;
      local.name.to_css(dest)?;
      dest.write_char(':')?;
      dest.whitespace()?;
      local.value.to_css(dest, true)?;
      dest.write_char(';')?;
    }
    if let Some(result) = &self.result {
      dest.newline()?;
      dest.write_str("result:")?;
      dest.whitespace()?;
      result.to_css(dest, false)?;
      if !dest.minify {
        dest.write_char(';')?;
      }
    }
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}

fn write_syntax<W>(syntax: &SyntaxString, dest: &mut Printer<W>) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  match syntax {
    SyntaxString::Universal => dest.write_char('*'),
    SyntaxString::Components(components) => {
      let mut first = true;
      for component in components {
        if first {
          first = false;
        } else {
          dest.delim('|', true)?;
        }
        component.to_css(dest)?;
      }
      Ok(())
    }
  }
}

enum TypeMatch<'i> {
  Folded(TokenList<'i>),
  Raw,
  NoMatch,
}

fn token_list_string(tokens: &TokenList) -> String {
  let mut s = String::new();
  let mut printer = Printer::new(&mut s, PrinterOptions::default());
  tokens.to_css(&mut printer, false).ok();
  s
}

fn number_token(value: f32, is_int: bool) -> TokenOrValue<'static> {
  TokenOrValue::Token(Token::Number {
    has_sign: false,
    value,
    int_value: if is_int { Some(value as i32) } else { None },
  })
}

fn component_token(component: &ParsedComponent) -> Option<TokenOrValue<'static>> {
  Some(match component {
    ParsedComponent::Number(n) => number_token(*n, n.fract() == 0.0),
    ParsedComponent::Integer(i) => number_token(*i as f32, true),
    ParsedComponent::Length(Length::Value(v)) => TokenOrValue::Length(v.clone()),
    ParsedComponent::Angle(a) => TokenOrValue::Angle(a.clone()),
    ParsedComponent::Time(t) => TokenOrValue::Time(t.clone()),
    ParsedComponent::Resolution(r) => TokenOrValue::Resolution(r.clone()),
    ParsedComponent::Color(c) => TokenOrValue::Color(c.clone()),
    ParsedComponent::Percentage(p) => TokenOrValue::Token(Token::Percentage {
      has_sign: false,
      unit_value: p.0,
      int_value: None,
    }),
    _ => return None,
  })
}

fn classify<'i>(tokens: &TokenList<'i>, syntax: &SyntaxString) -> TypeMatch<'i> {
  let string = token_list_string(tokens);
  let mut input = ParserInput::new(&string);
  let mut parser = Parser::new(&mut input);
  let result = match syntax.parse_value(&mut parser) {
    Ok(component) if parser.expect_exhausted().is_ok() => match component_token(&component) {
      Some(token) => TypeMatch::Folded(TokenList(vec![token])),
      None => TypeMatch::Raw,
    },
    _ => TypeMatch::NoMatch,
  };
  result
}

fn bind_default<'i>(tokens: &TokenList<'i>, ty: &Option<SyntaxString>) -> TokenList<'i> {
  match ty {
    Some(ty) => match classify(tokens, ty) {
      TypeMatch::Folded(folded) => folded,
      _ => tokens.clone(),
    },
    None => tokens.clone(),
  }
}

fn split_arguments<'i>(arguments: &TokenList<'i>) -> Vec<TokenList<'i>> {
  if arguments.0.is_empty() {
    return Vec::new();
  }

  let mut result = Vec::new();
  let mut current: Vec<TokenOrValue<'i>> = Vec::new();
  let mut depth = 0i32;
  for token in &arguments.0 {
    match token {
      TokenOrValue::Token(Token::ParenthesisBlock)
      | TokenOrValue::Token(Token::SquareBracketBlock)
      | TokenOrValue::Token(Token::CurlyBracketBlock) => {
        depth += 1;
        current.push(token.clone());
      }
      TokenOrValue::Token(Token::CloseParenthesis)
      | TokenOrValue::Token(Token::CloseSquareBracket)
      | TokenOrValue::Token(Token::CloseCurlyBracket) => {
        depth -= 1;
        current.push(token.clone());
      }
      TokenOrValue::Token(Token::Comma) if depth == 0 => {
        result.push(trim_token_list(std::mem::take(&mut current)));
      }
      _ => current.push(token.clone()),
    }
  }
  result.push(trim_token_list(current));
  result
}

fn trim_token_list<'i>(mut tokens: Vec<TokenOrValue<'i>>) -> TokenList<'i> {
  while tokens.last().map_or(false, |t| t.is_whitespace()) {
    tokens.pop();
  }
  let leading = tokens.iter().take_while(|t| t.is_whitespace()).count();
  tokens.drain(..leading);
  TokenList(tokens)
}

fn bind_parameter<'i>(
  parameter: &FunctionParameter<'i>,
  argument: Option<&TokenList<'i>>,
  name: &str,
) -> Result<TokenList<'i>, MinifyErrorKind> {
  let invalid = || MinifyErrorKind::InvalidFunctionArguments { name: name.to_owned() };
  let default = || parameter.default.as_ref().map(|default| bind_default(default, &parameter.ty));
  match (&parameter.ty, argument) {
    (Some(ty), Some(argument)) => match classify(argument, ty) {
      TypeMatch::Folded(folded) => Ok(folded),
      TypeMatch::Raw => Ok(argument.clone()),
      TypeMatch::NoMatch => default().ok_or_else(invalid),
    },
    (None, Some(argument)) => Ok(argument.clone()),
    (_, None) => default().ok_or_else(invalid),
  }
}

fn expand_call<'i>(
  call: &Function<'i>,
  call_site: &HashMap<String, TokenList<'i>>,
  functions: &HashMap<String, FunctionRule<'i>>,
  stack: &mut Vec<String>,
) -> Result<TokenList<'i>, MinifyErrorKind> {
  let name = call.name.0.as_ref();
  let function = &functions[name];
  if stack.iter().any(|n| n == name) {
    return Err(MinifyErrorKind::CircularFunction { name: name.to_owned() });
  }
  stack.push(name.to_owned());
  let result = evaluate_call(call, function, call_site, functions, stack);
  stack.pop();
  result
}

fn evaluate_call<'i>(
  call: &Function<'i>,
  function: &FunctionRule<'i>,
  call_site: &HashMap<String, TokenList<'i>>,
  functions: &HashMap<String, FunctionRule<'i>>,
  stack: &mut Vec<String>,
) -> Result<TokenList<'i>, MinifyErrorKind> {
  let name = function.name.0.as_ref();
  let arguments = split_arguments(&call.arguments);
  if arguments.len() > function.parameters.len() {
    return Err(MinifyErrorKind::InvalidFunctionArguments { name: name.to_owned() });
  }

  let mut scope = call_site.clone();
  for (index, parameter) in function.parameters.iter().enumerate() {
    let bound = bind_parameter(parameter, arguments.get(index), name)?;
    scope.insert(parameter.name.0.as_ref().to_owned(), bound);
  }
  for local in &function.locals {
    scope.insert(local.name.0.as_ref().to_owned(), local.value.clone());
  }

  let mut resolved = function.result.clone().unwrap_or_else(|| TokenList(Vec::new()));
  substitute(&mut resolved, &scope, call_site, functions, stack)?;

  if let Some(returns) = &function.returns {
    match classify(&resolved, returns) {
      TypeMatch::Folded(folded) => resolved = folded,
      TypeMatch::Raw => {}
      TypeMatch::NoMatch => return Err(MinifyErrorKind::InvalidFunctionResult { name: name.to_owned() }),
    }
  }

  Ok(resolved)
}

fn substitute<'i>(
  list: &mut TokenList<'i>,
  scope: &HashMap<String, TokenList<'i>>,
  call_site: &HashMap<String, TokenList<'i>>,
  functions: &HashMap<String, FunctionRule<'i>>,
  stack: &mut Vec<String>,
) -> Result<bool, MinifyErrorKind> {
  let mut changed = false;
  let mut i = 0;
  let mut seen: HashSet<String> = HashSet::new();
  while i < list.0.len() {
    let call = match &list.0[i] {
      TokenOrValue::Function(f) if functions.contains_key(f.name.0.as_ref()) => Some(f.clone()),
      _ => None,
    };
    if let Some(mut call) = call {
      substitute(&mut call.arguments, scope, call_site, functions, stack)?;
      let replacement = expand_call(&call, call_site, functions, stack)?;
      let count = replacement.0.len();
      list.0.splice(i..i + 1, replacement.0);
      changed = true;
      seen.clear();
      i += count;
      continue;
    }

    let substitution = match &list.0[i] {
      TokenOrValue::Var(v) => scope
        .get(v.name.ident.0.as_ref())
        .map(|value| (v.name.ident.0.as_ref().to_owned(), value.clone())),
      _ => None,
    };
    if let Some((variable, value)) = substitution {
      if seen.insert(variable) {
        list.0.splice(i..i + 1, value.0);
        changed = true;
        continue;
      }
    }

    match &mut list.0[i] {
      TokenOrValue::Var(v) => {
        if let Some(fallback) = &mut v.fallback {
          changed |= substitute(fallback, scope, call_site, functions, stack)?;
        }
      }
      TokenOrValue::Function(f) => {
        changed |= substitute(&mut f.arguments, scope, call_site, functions, stack)?;
      }
      _ => {}
    }
    seen.clear();
    i += 1;
  }
  Ok(changed)
}

pub(crate) struct Registries<'i> {
  functions: HashMap<String, FunctionRule<'i>>,
  registered: HashMap<String, RegisteredProperty<'i>>,
}

impl<'i> Registries<'i> {
  pub(crate) fn is_empty(&self) -> bool {
    self.functions.is_empty()
  }
}

struct RegisteredProperty<'i> {
  syntax: SyntaxString,
  initial: Option<TokenList<'i>>,
}

pub(crate) fn collect_registries<'i, T>(rules: &CssRuleList<'i, T>) -> Registries<'i> {
  let mut functions = HashMap::new();
  let mut registered = HashMap::new();
  for rule in &rules.0 {
    match rule {
      CssRule::Function(rule) => {
        functions.insert(rule.name.0.as_ref().to_owned(), rule.clone());
      }
      CssRule::Property(property) => {
        if let SyntaxString::Components(_) = &property.syntax {
          let initial = property
            .initial_value
            .as_ref()
            .and_then(component_token)
            .map(|token| TokenList(vec![token]));
          registered.insert(
            property.name.0.as_ref().to_owned(),
            RegisteredProperty {
              syntax: property.syntax.clone(),
              initial,
            },
          );
        }
      }
      _ => {}
    }
  }
  Registries { functions, registered }
}

pub(crate) fn inline_functions<'i, T>(
  rules: &mut CssRuleList<'i, T>,
  registries: &Registries<'i>,
) -> Result<(), MinifyError> {
  resolve_functions(rules, registries)?;
  rules.0.retain(|rule| !matches!(rule, CssRule::Function(_)));
  Ok(())
}

fn resolve_functions<'i, T>(
  rules: &mut CssRuleList<'i, T>,
  registries: &Registries<'i>,
) -> Result<(), MinifyError> {
  for rule in &mut rules.0 {
    match rule {
      CssRule::Style(style) => {
        resolve_declarations(&mut style.declarations, style.loc, registries)?;
        resolve_functions(&mut style.rules, registries)?;
      }
      CssRule::Nesting(nesting) => {
        resolve_declarations(&mut nesting.style.declarations, nesting.loc, registries)?;
        resolve_functions(&mut nesting.style.rules, registries)?;
      }
      CssRule::NestedDeclarations(rule) => resolve_declarations(&mut rule.declarations, rule.loc, registries)?,
      CssRule::Media(rule) => resolve_functions(&mut rule.rules, registries)?,
      CssRule::Supports(rule) => resolve_functions(&mut rule.rules, registries)?,
      CssRule::Container(rule) => resolve_functions(&mut rule.rules, registries)?,
      CssRule::Scope(rule) => resolve_functions(&mut rule.rules, registries)?,
      CssRule::LayerBlock(rule) => resolve_functions(&mut rule.rules, registries)?,
      CssRule::StartingStyle(rule) => resolve_functions(&mut rule.rules, registries)?,
      CssRule::MozDocument(rule) => resolve_functions(&mut rule.rules, registries)?,
      CssRule::Keyframes(rule) => {
        for keyframe in &mut rule.keyframes {
          resolve_declarations(&mut keyframe.declarations, rule.loc, registries)?;
        }
      }
      CssRule::Page(rule) => {
        resolve_declarations(&mut rule.declarations, rule.loc, registries)?;
        for margin in &mut rule.rules {
          resolve_declarations(&mut margin.declarations, margin.loc, registries)?;
        }
      }
      CssRule::CounterStyle(rule) => resolve_declarations(&mut rule.declarations, rule.loc, registries)?,
      CssRule::Viewport(rule) => resolve_declarations(&mut rule.declarations, rule.loc, registries)?,
      CssRule::PositionTry(rule) => resolve_declarations(&mut rule.declarations, rule.loc, registries)?,
      _ => {}
    }
  }
  Ok(())
}

fn resolve_declarations<'i>(
  block: &mut DeclarationBlock<'i>,
  loc: Location,
  registries: &Registries<'i>,
) -> Result<(), MinifyError> {
  let mut call_site: HashMap<String, TokenList<'i>> = HashMap::new();
  for property in block.declarations.iter().chain(block.important_declarations.iter()) {
    if let Property::Custom(custom) = property {
      if let CustomPropertyName::Custom(name) = &custom.name {
        let key = name.0.as_ref().to_owned();
        let value = match registries.registered.get(&key) {
          Some(registered) => match classify(&custom.value, &registered.syntax) {
            TypeMatch::Folded(folded) => folded,
            TypeMatch::Raw => custom.value.clone(),
            TypeMatch::NoMatch => registered.initial.clone().unwrap_or_else(|| custom.value.clone()),
          },
          None => custom.value.clone(),
        };
        call_site.insert(key, value);
      }
    }
  }

  let empty = HashMap::new();
  let properties = block.declarations.iter_mut().chain(block.important_declarations.iter_mut());
  for property in properties {
    let changed = match &mut *property {
      Property::Unparsed(unparsed) => substitute(
        &mut unparsed.value,
        &empty,
        &call_site,
        &registries.functions,
        &mut Vec::new(),
      ),
      Property::Custom(custom) => substitute(
        &mut custom.value,
        &empty,
        &call_site,
        &registries.functions,
        &mut Vec::new(),
      ),
      _ => Ok(false),
    }
    .map_err(|kind| ErrorWithLocation { kind, loc })?;

    if changed {
      reparse(property);
    }
  }
  Ok(())
}

#[cfg(feature = "into_owned")]
fn reparse(property: &mut Property<'_>) {
  if let Property::Unparsed(unparsed) = &mut *property {
    let css = token_list_string(&unparsed.value);
    let reparsed = Property::parse_string(unparsed.property_id.clone(), &css, ParserOptions::default())
      .ok()
      .map(|property| property.into_owned());
    if let Some(reparsed) = reparsed {
      *property = reparsed;
    }
  }
}

#[cfg(not(feature = "into_owned"))]
fn reparse(_property: &mut Property<'_>) {}
