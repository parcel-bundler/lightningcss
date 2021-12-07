use napi::{CallContext, JsObject, JsUndefined, JsString, Property, Result, Env};
use parcel_css::stylesheet::StyleSheet;
use crate::error::CompileError;
use crate::rule_list;
use std::rc::Rc;
use std::cell::RefCell;

/// https://drafts.csswg.org/cssom/#the-cssstylesheet-interface
struct CSSStyleSheet {
  stylesheet: Rc<RefCell<StyleSheet>>
}

#[js_function(0)]
fn constructor(ctx: CallContext) -> Result<JsUndefined> {
  let mut this: JsObject = ctx.this_unchecked();
  ctx.env.wrap(&mut this, CSSStyleSheet { stylesheet: Rc::new(RefCell::new(StyleSheet::empty("test.css".into()))) })?;
  ctx.env.get_undefined()
}

#[js_function(1)]
fn replace_sync(ctx: CallContext) -> Result<JsUndefined> {
  let text = ctx.get::<JsString>(0)?.into_utf8()?;
  let text = text.as_str()?;
  let this: JsObject = ctx.this_unchecked();
  let s: &mut CSSStyleSheet = ctx.env.unwrap(&this)?;
  s.stylesheet.borrow_mut().replace(text).map_err(|e| CompileError::from(e))?;
  ctx.env.get_undefined()
}

#[js_function(0)]
fn get_rules(ctx: CallContext) -> Result<JsObject> {
  let this: JsObject = ctx.this_unchecked();
  let s: &mut CSSStyleSheet = ctx.env.unwrap(&this)?;
  rule_list::create(ctx.env, s.stylesheet.clone())
}

pub fn init(exports: &mut JsObject, env: Env) -> Result<()> {
  let test_class = env
    .define_class("CSSStyleSheet", constructor, &[
      Property::new(&env, "replaceSync")?.with_method(replace_sync),
      Property::new(&env, "cssRules")?.with_getter(get_rules)
    ])?;
  exports.set_named_property("CSSStyleSheet", test_class)?;

  Ok(())
}
