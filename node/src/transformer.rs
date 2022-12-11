use std::ops::{Index, IndexMut};

use lightningcss::{
  properties::{
    custom::{Token, TokenOrValue},
    Property,
  },
  rules::{CssRule, CssRuleList},
  values::length::LengthValue,
  visitor::{Visit, VisitTypes, Visitor},
};
use napi::{Env, JsFunction, JsObject, Ref};
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;

pub struct JsVisitor {
  env: Env,
  visit_rule: Option<Ref<()>>,
  visit_media_rule: Option<Ref<()>>,
  visit_import_rule: Option<Ref<()>>,
  visit_style_rule: Option<Ref<()>>,
  visit_keyframes_rule: Option<Ref<()>>,
  visit_font_face_rule: Option<Ref<()>>,
  visit_font_palette_values_rule: Option<Ref<()>>,
  visit_page_rule: Option<Ref<()>>,
  visit_supports_rule: Option<Ref<()>>,
  visit_counter_style_rule: Option<Ref<()>>,
  visit_namespace_rule: Option<Ref<()>>,
  visit_custom_media_rule: Option<Ref<()>>,
  visit_layer_rule: Option<Ref<()>>,
  visit_property_rule: Option<Ref<()>>,
  property_map: Option<Ref<()>>,
  visit_container_rule: Option<Ref<()>>,
  visit_property: Option<Ref<()>>,
  visit_length: Option<Ref<()>>,
  visit_angle: Option<Ref<()>>,
  visit_ratio: Option<Ref<()>>,
  visit_resolution: Option<Ref<()>>,
  visit_time: Option<Ref<()>>,
  visit_color: Option<Ref<()>>,
  visit_image: Option<Ref<()>>,
  visit_url: Option<Ref<()>>,
  visit_media_query: Option<Ref<()>>,
  visit_supports_condition: Option<Ref<()>>,
  visit_variable: Option<Ref<()>>,
  visit_custom_ident: Option<Ref<()>>,
  visit_dashed_ident: Option<Ref<()>>,
  visit_function: Option<Ref<()>>,
  function_map: Option<Ref<()>>,
  visit_selector: Option<Ref<()>>,
  visit_token: Option<Ref<()>>,
  token_map: Option<Ref<()>>,
  types: VisitTypes,
  pub errors: Vec<napi::Error>,
}

// This is so that the visitor can work with bundleAsync.
// We ensure that we only call JsVisitor from the main JS thread.
unsafe impl Send for JsVisitor {}

impl Drop for JsVisitor {
  fn drop(&mut self) {
    macro_rules! drop {
      ($id: ident) => {
        if let Some(v) = &mut self.$id {
          drop(v.unref(self.env));
        }
      };
    }

    drop!(visit_rule);
    drop!(visit_media_rule);
    drop!(visit_import_rule);
    drop!(visit_style_rule);
    drop!(visit_keyframes_rule);
    drop!(visit_font_face_rule);
    drop!(visit_font_palette_values_rule);
    drop!(visit_page_rule);
    drop!(visit_supports_rule);
    drop!(visit_counter_style_rule);
    drop!(visit_namespace_rule);
    drop!(visit_custom_media_rule);
    drop!(visit_layer_rule);
    drop!(visit_property_rule);
    drop!(visit_container_rule);
    drop!(visit_property);
    drop!(property_map);
    drop!(visit_length);
    drop!(visit_angle);
    drop!(visit_ratio);
    drop!(visit_resolution);
    drop!(visit_time);
    drop!(visit_color);
    drop!(visit_image);
    drop!(visit_url);
    drop!(visit_media_query);
    drop!(visit_supports_condition);
    drop!(visit_variable);
    drop!(visit_custom_ident);
    drop!(visit_dashed_ident);
    drop!(visit_function);
    drop!(function_map);
    drop!(visit_selector);
    drop!(visit_token);
    drop!(token_map);
  }
}

impl JsVisitor {
  pub fn new(env: Env, visitor: JsObject) -> Self {
    let mut types = VisitTypes::empty();
    macro_rules! get {
      ($name: literal, $t: ident) => {{
        let res: Option<JsFunction> = visitor.get_named_property($name).ok();
        if res.is_some() {
          types |= VisitTypes::$t;
        }

        // We must create a reference so that the garbage collector doesn't destroy
        // the function before we try to call it (in the async bundle case).
        res.and_then(|res| env.create_reference(res).ok())
      }};
    }

    macro_rules! map {
      ($name: literal, $t: ident) => {{
        if let Ok(obj) = visitor.get_named_property::<JsObject>($name) {
          types |= VisitTypes::$t;
          env.create_reference(obj).ok()
        } else {
          None
        }
      }};
    }

    Self {
      env,
      visit_rule: get!("Rule", RULES),
      visit_media_rule: get!("MediaRule", RULES),
      visit_import_rule: get!("ImportRule", RULES),
      visit_style_rule: get!("StyleRule", RULES),
      visit_keyframes_rule: get!("KeyframesRule", RULES),
      visit_font_face_rule: get!("FontFaceRule", RULES),
      visit_font_palette_values_rule: get!("FontPaletteValuesRule", RULES),
      visit_page_rule: get!("PageRule", RULES),
      visit_supports_rule: get!("SupportsRule", RULES),
      visit_counter_style_rule: get!("CounterStyleRule", RULES),
      visit_namespace_rule: get!("NamespaceRule", RULES),
      visit_custom_media_rule: get!("CustomMediaRule", RULES),
      visit_layer_rule: get!("LayerRule", RULES),
      visit_property_rule: get!("PropertyRule", RULES),
      visit_container_rule: get!("ContainerRule", RULES),
      visit_property: get!("Property", PROPERTIES),
      property_map: map!("Property", PROPERTIES),
      visit_length: get!("Length", LENGTHS),
      visit_angle: get!("Angle", ANGLES),
      visit_ratio: get!("Ratio", RATIOS),
      visit_resolution: get!("Resolution", RESOLUTIONS),
      visit_time: get!("Time", TIMES),
      visit_color: get!("Color", COLORS),
      visit_image: get!("Image", IMAGES),
      visit_url: get!("Url", URLS),
      visit_media_query: get!("MediaQuery", MEDIA_QUERIES),
      visit_supports_condition: get!("SupportsCondition", SUPPORTS_CONDITIONS),
      visit_variable: get!("Variable", TOKENS),
      visit_custom_ident: get!("CustomIdent", CUSTOM_IDENTS),
      visit_dashed_ident: get!("DashedIdent", DASHED_IDENTS),
      visit_function: get!("Function", TOKENS),
      function_map: map!("Function", TOKENS),
      visit_selector: get!("Selector", SELECTORS),
      visit_token: get!("Token", TOKENS),
      token_map: map!("Token", TOKENS),
      types,
      errors: vec![],
    }
  }
}

macro_rules! unwrap {
  ($result: expr, $errors: expr) => {
    match $result {
      Ok(r) => r,
      Err(err) => {
        $errors.push(err);
        return;
      }
    }
  };
}

impl<'i> Visitor<'i> for JsVisitor {
  const TYPES: lightningcss::visitor::VisitTypes = VisitTypes::all();

  fn visit_types(&self) -> VisitTypes {
    self.types
  }

  fn visit_rule_list(&mut self, rules: &mut lightningcss::rules::CssRuleList<'i>) {
    // Similar to visit_list, but skips CssRule::Ignored rules.
    if self.types.contains(VisitTypes::RULES) {
      unwrap!(
        map(rules, |value| {
          if matches!(value, CssRule::Ignored) {
            return Ok(None);
          }

          let js_value = self.env.to_js_value(value)?;

          // Use a more specific visitor function if available, but fall back to visit_rule.
          let visit = match value {
            CssRule::Media(..) => self.visit_media_rule.as_ref(),
            CssRule::Import(..) => self.visit_import_rule.as_ref(),
            CssRule::Style(..) => self.visit_style_rule.as_ref(),
            CssRule::Keyframes(..) => self.visit_keyframes_rule.as_ref(),
            CssRule::FontFace(..) => self.visit_font_face_rule.as_ref(),
            CssRule::FontPaletteValues(..) => self.visit_font_palette_values_rule.as_ref(),
            CssRule::Page(..) => self.visit_page_rule.as_ref(),
            CssRule::Supports(..) => self.visit_supports_rule.as_ref(),
            CssRule::CounterStyle(..) => self.visit_counter_style_rule.as_ref(),
            CssRule::Namespace(..) => self.visit_namespace_rule.as_ref(),
            CssRule::CustomMedia(..) => self.visit_custom_media_rule.as_ref(),
            CssRule::LayerBlock(..) | CssRule::LayerStatement(..) => self.visit_layer_rule.as_ref(),
            CssRule::Property(..) => self.visit_property_rule.as_ref(),
            CssRule::Container(..) => self.visit_container_rule.as_ref(),
            // Deprecated or custom rules don't have separate methods.
            // Can use general Rule visitor for them.
            CssRule::MozDocument(..)
            | CssRule::Nesting(..)
            | CssRule::Viewport(..)
            | CssRule::Ignored
            | CssRule::Unknown(..)
            | CssRule::Custom(..) => None,
          }
          .or(self.visit_rule.as_ref());

          if let Some(visit) = visit {
            let visit: JsFunction = self.env.get_reference_value_unchecked(visit)?;
            let res = visit.call(None, &[js_value])?;
            self.env.from_js_value(res).map(serde_detach::detach)
          } else {
            Ok(None)
          }
        }),
        &mut self.errors
      )
    }

    rules.0.visit_children(self)
  }

  fn visit_declaration_block(&mut self, decls: &mut lightningcss::declaration::DeclarationBlock<'i>) {
    if self.types.contains(VisitTypes::PROPERTIES) {
      visit_property_list(
        &self.env,
        &mut decls.important_declarations,
        self.visit_property.as_ref(),
        self.property_map.as_ref(),
        &mut self.errors,
      );
      visit_property_list(
        &self.env,
        &mut decls.declarations,
        self.visit_property.as_ref(),
        self.property_map.as_ref(),
        &mut self.errors,
      );
    }
    decls.important_declarations.visit_children(self);
    decls.declarations.visit_children(self);
  }

  fn visit_length(&mut self, length: &mut LengthValue) {
    visit(&self.env, length, &self.visit_length, &mut self.errors)
  }

  fn visit_angle(&mut self, angle: &mut lightningcss::values::angle::Angle) {
    visit(&self.env, angle, &self.visit_angle, &mut self.errors)
  }

  fn visit_ratio(&mut self, ratio: &mut lightningcss::values::ratio::Ratio) {
    visit(&self.env, ratio, &self.visit_ratio, &mut self.errors)
  }

  fn visit_resolution(&mut self, resolution: &mut lightningcss::values::resolution::Resolution) {
    visit(&self.env, resolution, &self.visit_resolution, &mut self.errors)
  }

  fn visit_time(&mut self, time: &mut lightningcss::values::time::Time) {
    visit(&self.env, time, &self.visit_time, &mut self.errors)
  }

  fn visit_color(&mut self, color: &mut lightningcss::values::color::CssColor) {
    visit(&self.env, color, &self.visit_color, &mut self.errors)
  }

  fn visit_image(&mut self, image: &mut lightningcss::values::image::Image<'i>) {
    visit(&self.env, image, &self.visit_image, &mut self.errors);
    image.visit_children(self)
  }

  fn visit_url(&mut self, url: &mut lightningcss::values::url::Url<'i>) {
    visit(&self.env, url, &self.visit_url, &mut self.errors)
  }

  fn visit_media_list(&mut self, media: &mut lightningcss::media_query::MediaList<'i>) {
    visit_list(
      &self.env,
      &mut media.media_queries,
      &self.visit_media_query,
      &mut self.errors,
    );
    media.visit_children(self)
  }

  fn visit_supports_condition(&mut self, condition: &mut lightningcss::rules::supports::SupportsCondition<'i>) {
    visit(&self.env, condition, &self.visit_supports_condition, &mut self.errors);
    condition.visit_children(self)
  }

  fn visit_custom_ident(&mut self, ident: &mut lightningcss::values::ident::CustomIdent) {
    visit(&self.env, ident, &self.visit_custom_ident, &mut self.errors);
  }

  fn visit_dashed_ident(&mut self, ident: &mut lightningcss::values::ident::DashedIdent) {
    visit(&self.env, ident, &self.visit_dashed_ident, &mut self.errors);
  }

  fn visit_selector_list(&mut self, selectors: &mut lightningcss::selector::SelectorList<'i>) {
    visit_list(&self.env, &mut selectors.0, &self.visit_selector, &mut self.errors);
  }

  fn visit_token_list(&mut self, tokens: &mut lightningcss::properties::custom::TokenList<'i>) {
    if self.types.contains(VisitTypes::TOKENS) {
      macro_rules! get {
        ($name: ident) => {
          self.$name.as_ref().and_then(|p| self.env.get_reference_value_unchecked(p).ok())
        };
      }

      let visit_token: Option<JsFunction> = get!(visit_token);
      let token_map: Option<JsObject> = get!(token_map);
      let visit_function: Option<JsFunction> = get!(visit_function);
      let function_map: Option<JsObject> = get!(function_map);
      let visit_variable: Option<JsFunction> = get!(visit_variable);

      unwrap!(
        map(&mut tokens.0, |value| {
          let (visit_type, visit, js_value) = match value {
            TokenOrValue::Function(f) => {
              let visit = function_map
                .as_ref()
                .and_then(|m| m.get_named_property::<JsFunction>(f.name.0.as_ref()).ok());
              (visit, visit_function.as_ref(), self.env.to_js_value(f)?)
            }
            TokenOrValue::Var(v) => (None, visit_variable.as_ref(), self.env.to_js_value(v)?),
            TokenOrValue::Token(t) => {
              let name = match t {
                Token::Ident(_) => Some("ident"),
                Token::AtKeyword(_) => Some("at-keyword"),
                Token::Hash(_) => Some("hash"),
                Token::IDHash(_) => Some("id-hash"),
                Token::String(_) => Some("string"),
                Token::Number { .. } => Some("number"),
                Token::Percentage { .. } => Some("percentage"),
                Token::Dimension { .. } => Some("dimension"),
                _ => None,
              };
              let visit = if let Some(name) = name {
                token_map.as_ref().and_then(|m| m.get_named_property::<JsFunction>(name).ok())
              } else {
                None
              };
              (visit, visit_token.as_ref(), self.env.to_js_value(t)?)
            }
            _ => return Ok(None),
          };

          if let Some(visit) = visit_type.as_ref().or(visit) {
            let res = visit.call(None, &[js_value])?;
            self.env.from_js_value(res).map(serde_detach::detach)
          } else {
            Ok(None)
          }
        }),
        &mut self.errors
      );
    }

    tokens.visit_children(self)
  }
}

fn visit<V: Serialize + Deserialize<'static>>(
  env: &Env,
  value: &mut V,
  visit: &Option<Ref<()>>,
  errors: &mut Vec<napi::Error>,
) {
  if let Some(visit) = visit {
    let js_value = unwrap!(env.to_js_value(value), errors);
    let visit: JsFunction = unwrap!(env.get_reference_value_unchecked(visit), errors);
    let res = unwrap!(visit.call(None, &[js_value]), errors);
    let new_value: napi::Result<Option<V>> = env.from_js_value(res).map(serde_detach::detach);
    match new_value {
      Ok(Some(new_value)) => *value = new_value,
      Ok(None) => {}
      Err(err) => errors.push(err),
    }
  }
}

fn visit_list<V: Serialize + Deserialize<'static>, L: List<V>>(
  env: &Env,
  list: &mut L,
  visit: &Option<Ref<()>>,
  errors: &mut Vec<napi::Error>,
) {
  if let Some(visit) = visit {
    unwrap!(
      map(list, |value| {
        let js_value = env.to_js_value(value)?;
        let visit: JsFunction = env.get_reference_value_unchecked(visit)?;
        let res = visit.call(None, &[js_value])?;
        env.from_js_value(res).map(serde_detach::detach)
      }),
      errors
    )
  }
}

fn visit_property_list<'i>(
  env: &Env,
  list: &mut Vec<Property<'i>>,
  visit_property: Option<&Ref<()>>,
  property_map: Option<&Ref<()>>,
  errors: &mut Vec<napi::Error>,
) {
  let property_map: Option<JsObject> = property_map.and_then(|p| env.get_reference_value_unchecked(&p).ok());
  let visit_property: Option<JsFunction> =
    visit_property.and_then(|visit| env.get_reference_value_unchecked::<JsFunction>(visit).ok());

  unwrap!(
    map(list, |value| {
      let js_value = env.to_js_value(value)?;
      // Use a specific property visitor if available, or fall back to Property visitor.
      let visit = property_map
        .as_ref()
        .and_then(|m| m.get_named_property::<JsFunction>(value.property_id().name()).ok());
      if let Some(visit) = visit.as_ref().or(visit_property.as_ref()) {
        let res = visit.call(None, &[js_value])?;
        env.from_js_value(res).map(serde_detach::detach)
      } else {
        Ok(None)
      }
    }),
    errors
  )
}

fn map<V, L: List<V>, F: Fn(&V) -> napi::Result<Option<ValueOrVec<V>>>>(list: &mut L, f: F) -> napi::Result<()> {
  let mut i = 0;
  while i < list.len() {
    let value = &list[i];
    let new_value = f(value)?;
    match new_value {
      Some(ValueOrVec::Value(v)) => {
        list[i] = v;
        i += 1;
      }
      Some(ValueOrVec::Vec(vec)) => {
        if vec.is_empty() {
          list.remove(i);
        } else {
          let len = vec.len();
          list.replace(i, vec);
          i += len;
        }
      }
      None => {
        i += 1;
      }
    }
  }
  Ok(())
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
enum ValueOrVec<V> {
  Value(V),
  Vec(Vec<V>),
}

trait List<V>: Index<usize, Output = V> + IndexMut<usize, Output = V> {
  fn len(&self) -> usize;
  fn remove(&mut self, i: usize);
  fn replace(&mut self, i: usize, items: Vec<V>);
}

impl<V> List<V> for Vec<V> {
  fn len(&self) -> usize {
    Vec::len(self)
  }

  fn remove(&mut self, i: usize) {
    Vec::remove(self, i);
  }

  fn replace(&mut self, i: usize, items: Vec<V>) {
    self.splice(i..i + 1, items);
  }
}

impl<V, T: smallvec::Array<Item = V>> List<V> for SmallVec<T> {
  fn len(&self) -> usize {
    SmallVec::len(self)
  }

  fn remove(&mut self, i: usize) {
    SmallVec::remove(self, i);
  }

  fn replace(&mut self, i: usize, items: Vec<V>) {
    let len = items.len();
    let mut iter = items.into_iter();
    self[i] = iter.next().unwrap();
    if len > 1 {
      self.insert_many(i + 1, iter);
    }
  }
}

impl<'i, R> List<CssRule<'i, R>> for CssRuleList<'i, R> {
  fn len(&self) -> usize {
    self.0.len()
  }

  fn remove(&mut self, i: usize) {
    self[i] = CssRule::Ignored;
  }

  fn replace(&mut self, i: usize, items: Vec<CssRule<'i, R>>) {
    self.0.replace(i, items)
  }
}
