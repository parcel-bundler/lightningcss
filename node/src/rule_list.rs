use napi::{CallContext, JsNumber, JsObject, JsUndefined, JsUnknown, JsFunction, Property, Ref, Result, Env};
use std::cell::RefCell;
use parcel_css::stylesheet::StyleSheet;
use std::rc::Rc;
use crate::rule;

// TODO: this is probably not safe.
static mut CLASS: RefCell<Option<Ref<()>>> = RefCell::new(None);

#[js_function(0)]
fn constructor(ctx: CallContext) -> Result<JsUndefined> {
  ctx.env.get_undefined()
}

#[js_function(0)]
fn get_length(ctx: CallContext) -> Result<JsNumber> {
  let this: JsObject = ctx.this_unchecked();
  let stylesheet: &mut Rc<RefCell<StyleSheet>> = ctx.env.unwrap(&this)?;
  ctx.env.create_uint32(stylesheet.borrow().rules.0.len() as u32)
}

#[js_function(1)]
fn item(ctx: CallContext) -> Result<JsObject> {
  let this: JsObject = ctx.this_unchecked();
  let index = ctx.get::<JsNumber>(0)?.get_uint32()?;
  let stylesheet: &mut Rc<RefCell<StyleSheet>> = ctx.env.unwrap(&this)?;
  let r = stylesheet.borrow().rules.0[index as usize].clone();
  rule::create(ctx.env, r)
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

pub fn create(env: &Env, list: Rc<RefCell<StyleSheet>>) -> Result<JsObject> {
  let r = unsafe { CLASS.borrow() };
  let r = r.as_ref().unwrap();
  let c: JsFunction = env.get_reference_value(&r)?;
  let mut instance = c.new::<JsUnknown>(&[])?;
  env.wrap(&mut instance, list)?;
  Ok(instance)
}
