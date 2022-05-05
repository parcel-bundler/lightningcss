#![allow(dead_code)]

use std::{
  borrow::{BorrowMut, Cow},
  cell::RefCell,
  ops::{Deref, DerefMut},
  rc::Rc,
};

use cssparser::{Parser, ParserInput};
use napi::{bindgen_prelude::*, CallContext, JsNull, JsNumber, JsObject, JsString, JsUnknown, NapiValue, Ref};
use napi_derive::{js_function, napi};
use parcel_css::{
  declaration::DeclarationBlock,
  media_query::{MediaList, MediaQuery},
  properties::{Property, PropertyId},
  rules::{style::StyleRule, CssRule, CssRuleList},
  stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet},
  traits::{Parse, ToCss},
};

// https://drafts.csswg.org/cssom/#the-cssstylesheet-interface
#[napi(js_name = "CSSStyleSheet")]
struct CSSStyleSheet {
  stylesheet: StyleSheet<'static>,
  rules: Option<Reference<CSSRuleList>>,
}

#[napi]
impl CSSStyleSheet {
  #[napi(constructor)]
  pub fn new(env: Env) -> Self {
    CSSGroupingRule::init(env);
    CSSStyleSheet {
      stylesheet: StyleSheet::new(
        vec!["empty.css".into()],
        CssRuleList(Vec::new()),
        ParserOptions::default(),
      ),
      rules: None,
    }
  }

  #[napi]
  pub fn replace_sync(&mut self, env: Env, code: String) -> Result<()> {
    // Disconnect all existing rules from the stylesheet.
    if let Some(rules) = &mut self.rules {
      let rules = &mut **rules;
      for (index, rule) in rules.rules.iter_mut().enumerate() {
        if let Some(rule) = rule {
          let rule: &mut CSSRule = get_reference(env, rule)?;
          rule.inner = RuleInner::Disconnected(self.stylesheet.rules.0[index].clone());
        }
      }
    }

    self.stylesheet = StyleSheet::parse("empty.css", leak_str(code), ParserOptions::default()).unwrap();
    Ok(())
  }

  #[napi(getter)]
  pub fn css_rules(&mut self, env: Env, reference: Reference<CSSStyleSheet>) -> Result<Reference<CSSRuleList>> {
    if let Some(rules) = &self.rules {
      return rules.clone(env);
    }

    let rules = CSSRuleList {
      rule_list: RuleListReference::StyleSheet(
        reference
          .clone(env)?
          .share_with(env, |stylesheet| Ok(&mut stylesheet.stylesheet.rules))?,
      ),
      rules: Vec::new(),
      parent_rule: None,
      stylesheet_reference: reference,
    };

    self.rules = Some(CSSRuleList::into_reference(rules, env)?);
    self.rules.as_ref().unwrap().clone(env)
  }

  #[napi]
  pub fn insert_rule(&mut self, env: Env, rule: String, index: Option<u32>) -> Result<u32> {
    insert_rule(&mut self.stylesheet.rules.0, &mut self.rules, env, rule, index)
  }

  #[napi]
  pub fn add_rule(&mut self, env: Env, selector: String, style: String, index: Option<u32>) -> Result<i32> {
    // https://drafts.csswg.org/cssom/#dom-cssstylesheet-addrule

    let string = format!("{} {{ {} }}", selector, style);
    self.insert_rule(env, string, index)?;

    Ok(-1)
  }

  #[napi]
  pub fn delete_rule(&mut self, env: Env, index: u32) -> Result<()> {
    delete_rule(&mut self.stylesheet.rules.0, &mut self.rules, env, index)
  }

  #[napi]
  pub fn remove_rule(&mut self, env: Env, index: u32) -> Result<()> {
    self.delete_rule(env, index)
  }
}

fn insert_rule(
  rules: &mut Vec<CssRule<'static>>,
  js_rules: &mut Option<Reference<CSSRuleList>>,
  env: Env,
  rule: String,
  index: Option<u32>,
) -> Result<u32> {
  // https://drafts.csswg.org/cssom/#insert-a-css-rule
  let index = index.unwrap_or(0) as usize;
  if index > rules.len() {
    return Err(napi::Error::new(
      napi::Status::GenericFailure,
      "Index out of bounds".into(),
    ));
  }

  let new_rule = CssRule::parse_string(leak_str(rule), ParserOptions::default()).unwrap();

  // TODO: Check if new_rule can be inserted at position (e.g. @import)

  // Invalidate existing rule indices.
  if let Some(rules) = js_rules {
    let rules = &mut *rules;

    for rule in &rules.rules[index..] {
      if let Some(rule) = rule {
        let rule: &mut CSSRule = get_reference(env, rule)?;
        if let RuleInner::Connected { index, .. } = &mut rule.inner {
          *index += 1;
        }
      }
    }

    rules.rules.insert(index, None);
  }

  rules.insert(index, new_rule);
  Ok(index as u32)
}

fn delete_rule(
  rules: &mut Vec<CssRule<'static>>,
  js_rules: &mut Option<Reference<CSSRuleList>>,
  env: Env,
  index: u32,
) -> Result<()> {
  // https://drafts.csswg.org/cssom/#remove-a-css-rule
  let index = index as usize;
  if index > rules.len() {
    return Err(napi::Error::new(
      napi::Status::GenericFailure,
      "Index out of bounds".into(),
    ));
  }

  if let Some(rule_refs) = js_rules {
    let rule_objects = &mut *rule_refs;
    if index < rule_objects.rules.len() {
      if let Some(rule) = &rule_objects.rules[index] {
        let rule: &mut CSSRule = get_reference(env, rule)?;
        rule.inner = RuleInner::Disconnected(rules[index].clone());
      }

      for rule in &rule_objects.rules[index + 1..] {
        if let Some(rule) = rule {
          let rule: &mut CSSRule = get_reference(env, rule)?;
          if let RuleInner::Connected { index, .. } = &mut rule.inner {
            *index -= 1;
          }
        }
      }

      rule_objects.rules.remove(index);
    }
  }

  rules.remove(index);
  Ok(())
}

fn get_reference<T: napi::bindgen_prelude::FromNapiMutRef>(
  env: Env,
  reference: &Ref<()>,
) -> Result<&'static mut T> {
  let obj: JsUnknown = env.get_reference_value(reference)?;
  unsafe {
    let napi_value = napi::bindgen_prelude::ToNapiValue::to_napi_value(env.raw(), obj)?;
    T::from_napi_mut_ref(env.raw(), napi_value)
  }
}

enum RuleListReference {
  StyleSheet(SharedReference<CSSStyleSheet, &'static mut CssRuleList<'static>>),
  Rule(SharedReference<CSSGroupingRule, &'static mut CssRuleList<'static>>),
}

impl RuleListReference {
  fn clone(&self, env: Env) -> Result<Self> {
    match self {
      RuleListReference::StyleSheet(s) => Ok(RuleListReference::StyleSheet(s.clone(env)?)),
      RuleListReference::Rule(r) => Ok(RuleListReference::Rule(r.clone(env)?)),
    }
  }
}

impl Deref for RuleListReference {
  type Target = CssRuleList<'static>;

  fn deref(&self) -> &Self::Target {
    match self {
      RuleListReference::StyleSheet(s) => &**s,
      RuleListReference::Rule(r) => &**r,
    }
  }
}

impl DerefMut for RuleListReference {
  fn deref_mut(&mut self) -> &mut CssRuleList<'static> {
    match self {
      RuleListReference::StyleSheet(s) => &mut **s,
      RuleListReference::Rule(r) => &mut **r,
    }
  }
}

#[napi(js_name = "CSSRuleList")]
struct CSSRuleList {
  rule_list: RuleListReference,
  rules: Vec<Option<Ref<()>>>,
  parent_rule: Option<Reference<CSSGroupingRule>>,
  stylesheet_reference: Reference<CSSStyleSheet>,
}

#[napi]
impl CSSRuleList {
  #[napi(constructor)]
  pub fn new() -> Self {
    unreachable!()
  }

  #[napi(getter)]
  pub fn length(&self) -> u32 {
    self.rule_list.0.len() as u32
  }

  #[napi]
  pub fn item(&mut self, index: u32, env: Env) -> Result<JsUnknown> {
    let index = index as usize;
    if let Some(Some(rule)) = self.rules.get(index) {
      return env.get_reference_value(rule);
    }

    let rule = match self.rule_list.0.get(index) {
      Some(rule) => rule,
      None => return Ok(env.get_null()?.into_unknown()),
    };

    let css_rule = CSSRule {
      inner: RuleInner::Connected {
        rule_list: self.rule_list.clone(env)?,
        index,
      },
      parent_rule: if let Some(parent_rule) = &self.parent_rule {
        Some(parent_rule.clone(env)?)
      } else {
        None
      },
      parent_stylesheet: self.stylesheet_reference.clone(env)?,
    };

    let napi_value = match rule {
      CssRule::Style(_) => {
        let rule = CSSStyleRule::new(css_rule);
        unsafe { napi::bindgen_prelude::ToNapiValue::to_napi_value(env.raw(), rule)? }
      }
      CssRule::Media(_) => {
        let rule = CSSMediaRule {
          rule: CSSConditionRule {
            rule: CSSGroupingRule {
              rule: css_rule,
              rules: None,
            },
          },
          media: None,
        };
        unsafe { napi::bindgen_prelude::ToNapiValue::to_napi_value(env.raw(), rule)? }
      }
      _ => unreachable!(),
    };

    let unknown = unsafe { napi::JsUnknown::from_napi_value(env.raw(), napi_value)? };

    if self.rules.len() <= index {
      self.rules.resize_with(index + 1, || None);
    }

    self.rules[index] = Some(env.create_reference_with_refcount(&unknown, 0)?);
    Ok(unknown)
  }
}

enum RuleInner {
  Connected { rule_list: RuleListReference, index: usize },
  Disconnected(CssRule<'static>),
}

#[napi(js_name = "CSSRule")]
struct CSSRule {
  inner: RuleInner,
  parent_rule: Option<Reference<CSSGroupingRule>>,
  parent_stylesheet: Reference<CSSStyleSheet>,
}

#[napi]
impl CSSRule {
  #[napi(constructor)]
  pub fn new() -> Self {
    unreachable!()
  }

  #[napi(getter, js_name = "type")]
  pub fn kind(&self) -> u32 {
    match &*self.rule() {
      CssRule::Style(..) => 1,
      CssRule::Import(..) => 3,
      CssRule::Media(..) => 4,
      CssRule::FontFace(..) => 5,
      CssRule::Page(..) => 6,
      CssRule::Keyframes(..) => 7,
      CssRule::Namespace(..) => 10,
      CssRule::CounterStyle(..) => 11,
      CssRule::Supports(..) => 12,
      CssRule::Viewport(..) => 15,
      _ => 0,
    }
  }

  #[napi(getter)]
  pub fn css_text(&self) -> String {
    self.rule().to_css_string(PrinterOptions::default()).unwrap()
  }

  #[napi(setter)]
  pub fn set_css_text(&self) {
    // On setting the cssText attribute must do nothing.
  }

  fn rule(&self) -> &CssRule<'static> {
    match &self.inner {
      RuleInner::Connected { rule_list, index } => {
        // std::cell::Ref::map(stylesheet, |stylesheet| &stylesheet.stylesheet.rules.0[*index])
        &rule_list.0[*index]
      }
      RuleInner::Disconnected(rule) => &rule,
    }
  }

  fn rule_mut(&mut self) -> &mut CssRule<'static> {
    match &mut self.inner {
      RuleInner::Connected { rule_list, index } => &mut rule_list.0[*index],
      RuleInner::Disconnected(rule) => rule,
    }
  }

  #[napi(getter)]
  pub fn parent_style_sheet(&self, env: Env) -> Result<JsUnknown> {
    match &self.inner {
      RuleInner::Connected { .. } => unsafe {
        let value =
          napi::bindgen_prelude::ToNapiValue::to_napi_value(env.raw(), self.parent_stylesheet.clone(env))?;
        napi::JsUnknown::from_napi_value(env.raw(), value)
      },
      RuleInner::Disconnected(..) => Ok(env.get_null()?.into_unknown()),
    }
  }

  #[napi(getter)]
  pub fn parent_rule(&self, env: Env) -> Result<Option<Reference<CSSGroupingRule>>> {
    if let Some(parent) = &self.parent_rule {
      return Ok(Some(parent.clone(env)?));
    }

    Ok(None)
  }
}

#[napi(js_name = "CSSStyleRule")]
struct CSSStyleRule {
  rule: CSSRule,
  style: Option<Reference<CSSStyleDeclaration>>,
}

#[napi]
impl CSSStyleRule {
  fn new(rule: CSSRule) -> CSSStyleRule {
    CSSStyleRule { rule, style: None }
  }

  #[napi(constructor)]
  pub fn constructor() {
    unreachable!();
  }

  #[napi(getter)]
  pub fn selector_text(&self) -> String {
    match &*self.rule.rule() {
      CssRule::Style(style) => cssparser::ToCss::to_css_string(&style.selectors),
      _ => unreachable!(),
    }
  }

  #[napi(setter)]
  pub fn set_selector_text(&mut self, text: String) {
    match &mut *self.rule.rule_mut() {
      CssRule::Style(style) => {
        style.set_selector_text(leak_str(text)).unwrap();
      }
      _ => unreachable!(),
    }
  }

  #[napi(getter)]
  pub fn style(&mut self, env: Env, reference: Reference<CSSStyleRule>) -> Result<Reference<CSSStyleDeclaration>> {
    if let Some(rules) = &self.style {
      return rules.clone(env);
    }

    let style = CSSStyleDeclaration::into_reference(CSSStyleDeclaration { rule: reference }, env)?;
    self.style = Some(style.clone(env)?);
    Ok(style)
  }

  #[napi(setter)]
  pub fn set_style(&mut self, text: String) {
    match &mut *self.rule.rule_mut() {
      CssRule::Style(style) => {
        style.declarations = DeclarationBlock::parse_string(leak_str(text), ParserOptions::default()).unwrap();
      }
      _ => unreachable!(),
    };
  }

  fn rule(&self) -> &StyleRule<'static> {
    let rule = self.rule.rule();
    match rule {
      CssRule::Style(style) => style,
      _ => unreachable!(),
    }
  }

  fn rule_mut(&mut self) -> &mut StyleRule<'static> {
    let rule = self.rule.rule_mut();
    match rule {
      CssRule::Style(style) => style,
      _ => unreachable!(),
    }
  }
}

#[napi(js_name = "CSSStyleDeclaration")]
struct CSSStyleDeclaration {
  rule: Reference<CSSStyleRule>,
}

#[napi]
impl CSSStyleDeclaration {
  #[napi(constructor)]
  pub fn new() -> Self {
    unreachable!()
  }

  #[napi(getter)]
  pub fn parent_rule(&self, env: Env) -> Result<Reference<CSSStyleRule>> {
    self.rule.clone(env)
  }

  #[napi(getter)]
  pub fn css_text(&self) -> String {
    self.rule.rule().declarations.to_css_string(PrinterOptions::default()).unwrap()
  }

  #[napi(setter)]
  pub fn set_css_text(&mut self, text: String) {
    self.rule.set_style(text)
  }

  fn get_longhands(&self) -> Vec<String> {
    let rule = self.rule.rule();
    let mut longhands = Vec::new();
    for (property, _important) in rule.declarations.iter() {
      let property_id = property.property_id();
      if let Some(properties) = property_id.longhands() {
        longhands.extend(properties.iter().map(|property_id| property_id.name().to_owned()))
      } else {
        longhands.push(property_id.name().to_owned());
      }
    }

    return longhands;
  }

  #[napi(getter)]
  pub fn length(&self) -> u32 {
    return self.get_longhands().len() as u32;
  }

  #[napi]
  pub fn item(&self, index: u32) -> String {
    let mut longhands = self.get_longhands();
    let index = index as usize;
    if index < longhands.len() {
      return std::mem::take(&mut longhands[index]);
    }
    String::new()
  }

  #[napi]
  pub fn get_property_value(&self, property: String) -> String {
    let property_id = PropertyId::parse_string(&property).unwrap();
    let opts = PrinterOptions::default();

    if let Some((value, _important)) = self.rule.rule().declarations.get(&property_id) {
      return value.value_to_css_string(opts).unwrap();
    }

    String::new()
  }

  #[napi]
  pub fn get_property_priority(&mut self, property: String) -> &str {
    let property_id = PropertyId::parse_string(&property).unwrap();
    let important = if let Some((_value, important)) = self.rule.rule().declarations.get(&property_id) {
      important
    } else {
      false
    };

    if important {
      "important"
    } else {
      ""
    }
  }

  #[napi]
  pub fn set_property(&mut self, property: String, value: String, priority: Option<String>) {
    if value.is_empty() {
      self.remove_property(property);
      return;
    }

    let property =
      Property::parse_string(leak_str(property).into(), leak_str(value), ParserOptions::default()).unwrap();
    self.rule.rule_mut().declarations.set(
      property,
      if let Some(priority) = priority {
        priority.eq_ignore_ascii_case("important")
      } else {
        false
      },
    );
  }

  #[napi]
  pub fn remove_property(&mut self, property: String) -> String {
    let value = self.get_property_value(property.clone());

    let property_id = PropertyId::parse_string(&property).unwrap();
    self.rule.rule_mut().declarations.remove(&property_id);

    value
  }
}

// https://drafts.csswg.org/cssom-1/#the-cssgroupingrule-interface
#[napi(js_name = "CSSGroupingRule")]
struct CSSGroupingRule {
  rule: CSSRule,
  rules: Option<Reference<CSSRuleList>>,
}

#[napi]
impl CSSGroupingRule {
  #[napi(constructor)]
  pub fn new() {
    unreachable!()
  }

  fn init(env: Env) {
    let constructor_value = napi::bindgen_prelude::get_class_constructor("CSSGroupingRule\0").unwrap();
    let mut value = std::ptr::null_mut();
    unsafe { napi::sys::napi_get_reference_value(env.raw(), constructor_value, &mut value) };
    let constructor = unsafe { JsFunction::from_raw(env.raw(), value).unwrap() };
    let constructor = constructor.coerce_to_object().unwrap();
    let mut prototype: JsObject = constructor.get_named_property("prototype").unwrap();
    prototype
      .set_named_property(
        "insertRule",
        env.create_function("insertRule", grouping_rule_insert).unwrap(),
      )
      .unwrap();
  }

  #[napi(getter)]
  pub fn css_rules(&mut self, env: Env, reference: Reference<CSSGroupingRule>) -> Result<Reference<CSSRuleList>> {
    if let Some(rules) = &self.rules {
      return rules.clone(env);
    }

    let rules = CSSRuleList {
      rule_list: RuleListReference::Rule(reference.clone(env)?.share_with(
        env,
        |rule| match rule.rule.rule_mut() {
          CssRule::Media(media) => Ok(&mut media.rules),
          _ => unreachable!(),
        },
      )?),
      rules: Vec::new(),
      parent_rule: Some(reference),
      stylesheet_reference: self.rule.parent_stylesheet.clone(env)?,
    };

    self.rules = Some(CSSRuleList::into_reference(rules, env)?);
    self.rules.as_ref().unwrap().clone(env)
  }

  // #[napi]
  // pub fn insert_rule(&mut self, env: Env, rule: String, index: Option<u32>) -> Result<u32> {
  //   let rules = match self.rule.rule_mut() {
  //     CssRule::Media(media) => &mut media.rules.0,
  //     _ => unreachable!(),
  //   };
  //   insert_rule(rules, &mut self.rules, env, rule, index)
  // }

  #[napi]
  pub fn delete_rule(&mut self, env: Env, index: u32) -> Result<()> {
    let rules = match self.rule.rule_mut() {
      CssRule::Media(media) => &mut media.rules.0,
      _ => unreachable!(),
    };
    delete_rule(rules, &mut self.rules, env, index)
  }
}

// Inheritance doesn't work with methods. v8 throws "Illegal invocation" errors due to signature checks.
// https://github.com/nodejs/node-addon-api/issues/246
// Instead, define a pure JS function here and assign it to the prototype of the class manually.
#[js_function(2)]
fn grouping_rule_insert(ctx: CallContext) -> Result<JsNumber> {
  let this: JsObject = ctx.this()?;
  // This is probably extremely unsafe.
  let napi_value = unsafe { napi::bindgen_prelude::ToNapiValue::to_napi_value(ctx.env.raw(), this).unwrap() };
  let rule = unsafe { CSSGroupingRule::from_napi_mut_ref(ctx.env.raw(), napi_value).unwrap() };
  let rules = match rule.rule.rule_mut() {
    CssRule::Media(media) => &mut media.rules.0,
    _ => unreachable!(),
  };
  let new_rule: JsString = ctx.get(0)?;
  let utf8 = new_rule.into_utf8()?;
  let new_rule = utf8.into_owned()?;
  let index: Option<u32> = if ctx.length > 1 {
    Some(ctx.get::<JsNumber>(1)?.get_uint32()?)
  } else {
    None
  };
  let res = insert_rule(rules, &mut rule.rules, *ctx.env, new_rule, index);
  if let Ok(res) = res {
    ctx.env.create_uint32(res)
  } else {
    Err(res.unwrap_err())
  }
}

// https://drafts.csswg.org/css-conditional-3/#cssconditionrule
#[napi(js_name = "CSSConditionRule")]
struct CSSConditionRule {
  rule: CSSGroupingRule,
}

#[napi]
impl CSSConditionRule {
  #[napi(constructor)]
  pub fn new() {
    unreachable!()
  }

  #[napi(getter)]
  pub fn condition_text(&self) -> Result<String> {
    match self.rule.rule.rule() {
      CssRule::Media(media) => Ok(media.query.to_css_string(PrinterOptions::default()).unwrap()),
      CssRule::Supports(supports) => Ok(supports.condition.to_css_string(PrinterOptions::default()).unwrap()),
      _ => Err(napi::Error::new(
        napi::Status::InvalidArg,
        "Not a conditional rule".into(),
      )),
    }
  }
}

// https://drafts.csswg.org/css-conditional-3/#cssmediarule
#[napi(js_name = "CSSMediaRule")]
struct CSSMediaRule {
  rule: CSSConditionRule,
  media: Option<Reference<JSMediaList>>,
}

#[napi]
impl CSSMediaRule {
  #[napi(constructor)]
  pub fn new() {
    unreachable!()
  }

  #[napi(getter)]
  pub fn media(&mut self, env: Env, reference: Reference<CSSMediaRule>) -> Result<Reference<JSMediaList>> {
    if let Some(media) = &self.media {
      return media.clone(env);
    }

    let media = JSMediaList::into_reference(
      JSMediaList {
        media_list: reference.share_with(env, |rule| match rule.rule.rule.rule.rule_mut() {
          CssRule::Media(media) => Ok(&mut media.query),
          _ => unreachable!(),
        })?,
      },
      env,
    )?;
    self.media = Some(media.clone(env)?);
    Ok(media)
  }
}

#[napi(js_name = "MediaList")]
struct JSMediaList {
  media_list: SharedReference<CSSMediaRule, &'static mut MediaList<'static>>,
}

#[napi]
impl JSMediaList {
  #[napi(constructor)]
  pub fn new() {
    unreachable!()
  }

  #[napi(getter)]
  pub fn media_text(&self) -> String {
    self.media_list.to_css_string(PrinterOptions::default()).unwrap()
  }

  #[napi(getter)]
  pub fn length(&self) -> u32 {
    self.media_list.media_queries.len() as u32
  }

  #[napi]
  pub fn item(&self, index: u32) -> Option<String> {
    if let Some(query) = self.media_list.media_queries.get(index as usize) {
      return Some(query.to_css_string(PrinterOptions::default()).unwrap());
    }

    None
  }

  #[napi]
  pub fn append_medium(&mut self, medium: String) {
    if let Ok(query) = MediaQuery::parse_string(leak_str(medium)) {
      if self.media_list.media_queries.contains(&query) {
        return;
      }

      self.media_list.media_queries.push(query);
    }
  }

  #[napi]
  pub fn delete_medium(&mut self, medium: String) -> Result<()> {
    if let Ok(query) = MediaQuery::parse_string(leak_str(medium)) {
      let queries = &mut self.media_list.media_queries;
      let len = queries.len();
      queries.retain(|q| *q != query);
      if queries.len() == len {
        return Err(napi::Error::new(napi::Status::GenericFailure, "Rule not found".into()));
      }
    }

    Ok(())
  }
}

fn leak_str(string: String) -> &'static str {
  let res = unsafe {
    let slice = std::slice::from_raw_parts(string.as_ptr(), string.len());
    std::str::from_utf8_unchecked(slice)
  };
  std::mem::forget(string);
  res
}