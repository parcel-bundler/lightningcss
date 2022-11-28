use crate::threadsafe_function::{ThreadSafeCallContext, ThreadsafeFunction, ThreadsafeFunctionCallMode};
use lightningcss::{
  values::length::LengthValue,
  visitor::{Visit, VisitTypes, Visitor},
};
use napi::{Env, JsError, JsFunction, JsObject, JsString, NapiRaw};
use serde::{Deserialize, Serialize};

pub struct JsVisitor {
  env: Env,
  visit_rule: Option<JsFunction>,
  visit_property: Option<JsFunction>,
  visit_length: Option<JsFunction>,
  visit_angle: Option<JsFunction>,
  visit_ratio: Option<JsFunction>,
  visit_resolution: Option<JsFunction>,
  visit_time: Option<JsFunction>,
  visit_color: Option<JsFunction>,
  visit_image: Option<JsFunction>,
  visit_url: Option<JsFunction>,
  visit_media_query: Option<JsFunction>,
  visit_supports_condition: Option<JsFunction>,
  visit_variable: Option<JsFunction>,
  visit_custom_ident: Option<JsFunction>,
  visit_dashed_ident: Option<JsFunction>,
  visit_function: Option<JsFunction>,
  visit_selector: Option<JsFunction>,
  visit_token: Option<JsFunction>,
  types: VisitTypes,
  pub errors: Vec<napi::Error>,
}

impl JsVisitor {
  pub fn new(env: Env, visitor: JsObject) -> Self {
    let mut types = VisitTypes::empty();
    macro_rules! get {
      ($name: literal, $t: ident) => {{
        let res = visitor.get_named_property($name).ok();
        if res.is_some() {
          types |= VisitTypes::$t;
        }
        res
      }};
    }

    Self {
      env,
      visit_rule: get!("visitRule", RULES),
      visit_property: get!("visitProperty", PROPERTIES),
      visit_length: get!("visitLength", LENGTHS),
      visit_angle: get!("visitAngle", ANGLES),
      visit_ratio: get!("visitRatio", RATIOS),
      visit_resolution: get!("visitResolution", RESOLUTIONS),
      visit_time: get!("visitTime", TIMES),
      visit_color: get!("visitColor", COLORS),
      visit_image: get!("visitImage", IMAGES),
      visit_url: get!("visitUrl", URLS),
      visit_media_query: get!("visitMediaQuery", MEDIA_QUERIES),
      visit_supports_condition: get!("visitSupportsCondition", SUPPORTS_CONDITIONS),
      visit_variable: get!("visitVariable", VARIABLES),
      visit_custom_ident: get!("visitCustomIdent", CUSTOM_IDENTS),
      visit_dashed_ident: get!("visitDashedIdent", DASHED_IDENTS),
      visit_function: get!("visitFunction", FUNCTIONS),
      visit_selector: get!("visitSelector", SELECTORS),
      visit_token: get!("visitToken", TOKENS),
      types,
      errors: vec![],
    }
  }
}

impl<'i> Visitor<'i> for JsVisitor {
  const TYPES: lightningcss::visitor::VisitTypes = VisitTypes::all();

  fn visit_rule(&mut self, rule: &mut lightningcss::rules::CssRule<'i>) {
    visit(&self.env, rule, &self.visit_rule, &mut self.errors);
    rule.visit_children(self)
  }

  fn visit_property(&mut self, property: &mut lightningcss::properties::Property<'i>) {
    visit(&self.env, property, &self.visit_property, &mut self.errors);
    property.visit_children(self)
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

  fn visit_media_query(&mut self, query: &mut lightningcss::media_query::MediaQuery<'i>) {
    visit(&self.env, query, &self.visit_media_query, &mut self.errors);
    query.visit_children(self)
  }

  fn visit_supports_condition(&mut self, condition: &mut lightningcss::rules::supports::SupportsCondition<'i>) {
    visit(&self.env, condition, &self.visit_supports_condition, &mut self.errors);
    condition.visit_children(self)
  }

  fn visit_variable(&mut self, var: &mut lightningcss::properties::custom::Variable<'i>) {
    visit(&self.env, var, &self.visit_variable, &mut self.errors);
    var.visit_children(self)
  }

  fn visit_custom_ident(&mut self, ident: &mut lightningcss::values::ident::CustomIdent) {
    visit(&self.env, ident, &self.visit_custom_ident, &mut self.errors);
  }

  fn visit_dashed_ident(&mut self, ident: &mut lightningcss::values::ident::DashedIdent) {
    visit(&self.env, ident, &self.visit_dashed_ident, &mut self.errors);
  }

  fn visit_function(&mut self, function: &mut lightningcss::properties::custom::Function<'i>) {
    visit(&self.env, function, &self.visit_function, &mut self.errors);
    function.visit_children(self)
  }

  fn visit_selector(&mut self, _selector: &mut lightningcss::selector::Selector<'i>) {
    visit(&self.env, selector, &self.visit_selector, &mut self.errors)
  }

  fn visit_token(&mut self, token: &mut lightningcss::properties::custom::TokenOrValue<'i>) {
    visit(&self.env, token, &self.visit_token, &mut self.errors);
    token.visit_children(self)
  }
}

fn visit<V: Serialize + Deserialize<'static>>(
  env: &Env,
  value: &mut V,
  visit: &Option<JsFunction>,
  errors: &mut Vec<napi::Error>,
) {
  if let Some(visit) = visit {
    let js_value = env.to_js_value(value).unwrap();
    let res = visit.call(None, &[js_value]).unwrap();
    match env.from_js_value(res).map(serde_detach::detach) {
      Ok(new_value) => *value = new_value,
      Err(err) => errors.push(err),
    }
  }
}
