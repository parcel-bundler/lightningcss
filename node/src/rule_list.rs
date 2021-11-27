use napi::{CallContext, sys, JsNumber, JsObject, NapiRaw, NapiValue, JsUndefined, JsUnknown, JsFunction, Property, Ref, Result, Env};
use std::cell::RefCell;
use parcel_css::stylesheet::StyleSheet;
use std::rc::Rc;
use crate::rule;

// TODO: this is probably not safe.
static mut CLASS: RefCell<Option<Ref<()>>> = RefCell::new(None);

struct CSSRuleList {
  stylesheet: Rc<RefCell<StyleSheet>>,
  rules: Vec<sys::napi_ref>
}

#[js_function(0)]
fn constructor(ctx: CallContext) -> Result<JsUndefined> {
  ctx.env.get_undefined()
}

#[js_function(0)]
fn get_length(ctx: CallContext) -> Result<JsNumber> {
  let this: JsObject = ctx.this_unchecked();
  let list: &mut CSSRuleList = ctx.env.unwrap(&this)?;
  ctx.env.create_uint32(list.stylesheet.borrow().rules.0.len() as u32)
}

#[js_function(1)]
fn item(ctx: CallContext) -> Result<JsObject> {
  let this: JsObject = ctx.this_unchecked();
  let index = ctx.get::<JsNumber>(0)?.get_uint32()? as usize;
  let list: &mut CSSRuleList = ctx.env.unwrap(&this)?;

  // See if we already have a JS object created for this rule to preserve referential equality.
  // e.g. rules.item(0) === rules.item(0)
  // This drops down to low level C bindings for napi because there is currently no way
  // to create a weak reference in napi-rs.
  match &list.rules.get(index) {
    Some(r) if !r.is_null() => {
      let mut js_value = std::ptr::null_mut();
      unsafe {
        sys::napi_get_reference_value(ctx.env.raw(), **r, &mut js_value);
        if !js_value.is_null() {
          return JsObject::from_raw(ctx.env.raw(), js_value)
        }
      };
    }
    _ => {}
  };

  let r = list.stylesheet.borrow().rules.0[index].clone();
  let obj = rule::create(ctx.env, r)?;
  while list.rules.len() <= index {
    list.rules.push(std::ptr::null_mut());
  }
  unsafe {
    sys::napi_create_reference(ctx.env.raw(), obj.raw(), 0, &mut list.rules[index]);
  };
  Ok(obj)
}

pub fn init(exports: &mut JsObject, env: Env) -> Result<()> {
  let stylesheet_class = env
    .define_class("CSSRuleList", constructor, &[
      Property::new(&env, "length")?.with_getter(get_length),
      Property::new(&env, "item")?.with_method(item)
    ])?;
  let mut c = unsafe { CLASS.borrow_mut() };
  *c = Some(env.create_reference(&stylesheet_class)?);
  exports.set_named_property("CSSRuleList", stylesheet_class)?;

  Ok(())
}

pub fn create(env: &Env, stylesheet: Rc<RefCell<StyleSheet>>) -> Result<JsObject> {
  let r = unsafe { CLASS.borrow() };
  let r = r.as_ref().unwrap();
  let c: JsFunction = env.get_reference_value(&r)?;
  let mut instance = c.new::<JsUnknown>(&[])?;
  env.wrap(&mut instance, CSSRuleList { stylesheet, rules: Vec::new() })?;
  Ok(instance)
}
