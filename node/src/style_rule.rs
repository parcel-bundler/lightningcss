use napi::{CallContext, JsString, JsObject, JsUndefined, JsUnknown, JsFunction, Property, Ref, Result, Env};
use std::cell::RefCell;
use parcel_css::rules::CssRule;
use crate::rule;
use cssparser::ToCss;

static mut CLASS: RefCell<Option<Ref<()>>> = RefCell::new(None);

#[js_function(0)]
fn constructor(ctx: CallContext) -> Result<JsUndefined> {
  ctx.env.get_undefined()
}

#[js_function(0)]
fn get_selector_text(ctx: CallContext) -> Result<JsString> {
  let this: JsObject = ctx.this_unchecked();
  let rule: &mut CssRule = ctx.env.unwrap(&this)?;
  let text = match rule {
    CssRule::Style(style) => style.borrow().selectors.to_css_string(),
    _ => unreachable!()
  };
  ctx.env.create_string_from_std(text)
}

pub fn init(exports: &mut JsObject, env: Env) -> Result<()> {
  let stylesheet_class = env
    .define_class("CSSStyleRule", constructor, &[
      Property::new(&env, "selectorText")?.with_getter(get_selector_text)
    ])?;
  let stylesheet_class = rule::inherit(&env, stylesheet_class)?;
  let mut c = unsafe { CLASS.borrow_mut() };
  *c = Some(env.create_reference(&stylesheet_class)?);
  exports.set_named_property("CSSStyleRule", stylesheet_class)?;

  Ok(())
}

pub fn create(env: &Env, list: CssRule) -> Result<JsObject> {
  let r = unsafe { CLASS.borrow() };
  let r = r.as_ref().unwrap();
  let c: JsFunction = env.get_reference_value(&r)?;
  let mut instance = c.new::<JsUnknown>(&[])?;
  env.wrap(&mut instance, list)?;
  Ok(instance)
}
