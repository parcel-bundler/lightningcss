use napi::{CallContext, JsString, JsObject, JsUndefined, JsFunction, Property, Ref, Result, Env};
use std::cell::RefCell;
use parcel_css::rules::CssRule;
use std::convert::TryInto;
use crate::style_rule;

static mut CLASS: RefCell<Option<Ref<()>>> = RefCell::new(None);

#[js_function(0)]
fn constructor(ctx: CallContext) -> Result<JsUndefined> {
  ctx.env.get_undefined()
}

#[js_function(0)]
fn get_css_text(ctx: CallContext) -> Result<JsString> {
  let this: JsObject = ctx.this_unchecked();
  let rule: &mut CssRule = ctx.env.unwrap(&this)?;
  let text = rule.to_css_string();
  ctx.env.create_string_from_std(text)
}

pub fn init(exports: &mut JsObject, env: Env) -> Result<()> {
  let stylesheet_class = env
    .define_class("CSSRule", constructor, &[
      Property::new(&env, "cssText")?.with_getter(get_css_text)
    ])?;
  let mut c = unsafe { CLASS.borrow_mut() };
  *c = Some(env.create_reference(&stylesheet_class)?);
  exports.set_named_property("CSSRule", stylesheet_class)?;

  Ok(())
}

pub fn inherit(env: &Env, obj: JsFunction) -> Result<JsFunction> {
  let r = unsafe { CLASS.borrow() };
  let r = r.as_ref().unwrap();
  let super_class: JsFunction = env.get_reference_value(&r)?;
  let super_class_obj: JsObject = super_class.coerce_to_object()?;
  let class_obj: JsObject = obj.coerce_to_object()?;

  // Based on https://github.com/nodejs/node-addon-api/issues/229#issuecomment-383583352
  let global = env.get_global()?;
  let object: JsFunction = global.get_named_property("Object")?;
  let object = object.coerce_to_object()?;
  let set_proto: JsFunction = object.get_named_property("setPrototypeOf")?;
  let proto: JsObject = class_obj.get_named_property("prototype")?;
  let super_proto: JsObject = super_class_obj.get_named_property("prototype")?;

  set_proto.call(None, &[&proto, &super_proto])?;
  set_proto.call(None, &[&class_obj, &super_class_obj])?;
  class_obj.into_unknown().try_into()
}

pub fn create(env: &Env, rule: CssRule) -> Result<JsObject> {
  match rule {
    CssRule::Style(_) => style_rule::create(env, rule),
    _ => unimplemented!()
  } 
}
