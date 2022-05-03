"use strict";

const { CSSStyleSheet, CSSStyleRule, CSSRule } = require('./');

Object.setPrototypeOf(CSSStyleRule.prototype, CSSRule.prototype);

let stylesheet = new CSSStyleSheet();
stylesheet.replaceSync(`
.foo {
  border: 1px solid black;
}
`);

console.log(stylesheet.cssRules == stylesheet.cssRules);
console.log(stylesheet.cssRules.item(0) == stylesheet.cssRules.item(0))

let rule = stylesheet.cssRules.item(0);
console.log(rule, rule instanceof CSSStyleRule, rule instanceof CSSRule);
console.log(rule.parentStyleSheet, rule.parentStyleSheet === stylesheet);
console.log(rule.cssText);
console.log(rule.type)
rule.cssText = "hi";

console.log(rule.selectorText);
rule.selectorText = ".bar";
// console.log(rule.style.getPropertyValue('animation-duration'));
console.log(rule.style.length);
console.log(rule.style.item(0))
console.log(rule.style.parentRule, rule.style.parentRule === rule);
// console.log(rule.style.getPropertyPriority('animation-duration'));
// rule.style.removeProperty('animation-name');
// console.log(rule.style.length);
// console.log(rule.style.getPropertyValue('animation-name'));
// console.log(rule.style.getPropertyValue('animation-duration'));
// console.log(rule.style.getPropertyValue('animation'));

rule.style.setProperty('border-color', 'red', '');
console.log(rule.cssText);

rule.style = "background: green";
console.log(rule.cssText);

stylesheet.insertRule(".hi { color: purple }", 0);
let newRule = stylesheet.cssRules.item(0);
console.log(stylesheet.cssRules.item(0).cssText);
console.log(stylesheet.cssRules.item(1).cssText);

stylesheet.deleteRule(0);
console.log(stylesheet.cssRules.item(0).cssText);
console.log(stylesheet.cssRules.item(1));
console.log(newRule.cssText);

// rule.style.setProperty('padding-inline', '9px', '');
// console.log(rule.style.getPropertyValue('border'))
// rule.style.setProperty('background-color', 'yellow', '');
// rule.style.setProperty('font-family', 'Helvetica', '');
// console.log(rule.style.getPropertyValue('background'));
// console.log(rule.cssText);

// let unit = new CSSUnitValue(2, 'px');
// console.log(unit.value, unit.unit, unit.toString());
// // console.log(unit.add(new CSSUnitValue(2, 'in')));

// let v = CSSNumericValue.parse('calc(2px + 1em)');//.add(CSSNumericValue.parse('1in'));
// console.log(v)
// // console.log(v.toUnitValue().value);
// let s = v.toSum().items;
// console.log(s.first == s.first);
