const { suite } = require('uvu');
const { CSSStyleSheet, CSSStyleRule, CSSRule, CSSStyleDeclaration } = require('../');
const assert = require('assert');

Object.setPrototypeOf(CSSStyleRule.prototype, CSSRule.prototype);

function run(name, fn) {
  let test = suite(name);
  fn(test);
  test.run();
}

function createStyleSheet() {
  let stylesheet = new CSSStyleSheet();
  stylesheet.replaceSync(`
.foo {
  border: 1px solid black;
  color: green !important;
}
`);
  return stylesheet;
}

function styleRule() {
  return createStyleSheet().cssRules.item(0);
}

function declaration() {
  return styleRule().style;
}

run('CSSStyleSheet', test => {
  test('cssRules are referentially equal', () => {
    let stylesheet = createStyleSheet();
    assert.equal(stylesheet.cssRules, stylesheet.cssRules);
  });

  test('insertRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    let index = stylesheet.insertRule('.bar { color: red }');
    assert.equal(index, 0);
    assert.equal(stylesheet.cssRules.length, 2);
    assert.equal(stylesheet.cssRules.item(1), rule);
    let newRule = stylesheet.cssRules.item(0);
    assert(newRule instanceof CSSStyleRule);
    assert.equal(newRule.parentStyleSheet, stylesheet);
    assert.equal(newRule.cssText, `.bar {
  color: red;
}`);
    assert.equal(rule.cssText, `.foo {
  border: 1px solid #000;
  color: green !important;
}`);

    stylesheet.insertRule('baz { color: green }', 1);
    assert.equal(stylesheet.cssRules.length, 3);
    assert.equal(stylesheet.cssRules.item(0), newRule);
    assert.equal(stylesheet.cssRules.item(2), rule);

    assert.throws(() => stylesheet.insertRule('.bar { color: red }', 10));
    assert.throws(() => stylesheet.insertRule('.bar { color: red }', -1));
  });

  test('addRule', () => {
    let stylesheet = createStyleSheet();
    stylesheet.addRule('.bar', 'color: red');
    assert.equal(stylesheet.cssRules.length, 2);
    let newRule = stylesheet.cssRules.item(0);
    assert.equal(newRule.cssText, `.bar {
  color: red;
}`);
  });

  test('deleteRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    stylesheet.deleteRule(0);
    assert.equal(stylesheet.cssRules.length, 0);
    assert.equal(rule.parentStyleSheet, null);
    assert.equal(rule.cssText, `.foo {
  border: 1px solid #000;
  color: green !important;
}`);

    assert.throws(() => stylesheet.deleteRule(10));
    assert.throws(() => stylesheet.deleteRule(-1));
  });

  test('replaceSync', () => {
    let stylesheet = createStyleSheet();
    let rules = stylesheet.cssRules;
    let rule = rules.item(0);

    stylesheet.replaceSync('.bar { color: red }');

    assert.equal(stylesheet.cssRules, rules);
    assert.equal(rules.length, 1);
    assert.equal(rule.parentStyleSheet, null);
    assert.equal(rule.cssText, `.foo {
  border: 1px solid #000;
  color: green !important;
}`);
  });
});

run('CSSRuleList', test => {
  test('item() returns referentially equal objects', () => {
    let stylesheet = createStyleSheet();
    assert.equal(stylesheet.cssRules.item(0), stylesheet.cssRules.item(0));
  });

  test('has correct length', () => {
    let stylesheet = createStyleSheet();
    assert.equal(stylesheet.cssRules.length, 1);
  });

  test('item() returns correct rule subclasses', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    assert(rule instanceof CSSStyleRule);
    assert(rule instanceof CSSRule);
  });
});

run('CSSRule', test => {
  test('parentStyleSheet equals stylesheet', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    assert.equal(rule.parentStyleSheet, stylesheet);
  });

  test('cssText getter', () => {
    assert.equal(styleRule().cssText, `.foo {
  border: 1px solid #000;
  color: green !important;
}`);
  });

  test('cssText setter', () => {
    let rule = styleRule();
    rule.cssText = 'foo';
    assert.equal(rule.cssText, `.foo {
  border: 1px solid #000;
  color: green !important;
}`);
  });

  test('type', () => {
    assert.equal(styleRule().type, 1);
  });
});

run('CSSStyleRule', test => {
  test('selectorText getter', () => {
    assert.equal(styleRule().selectorText, '.foo');
  });

  test('selectorText setter', () => {
    let rule = styleRule();
    rule.selectorText = '.bar';
    assert.equal(rule.selectorText, '.bar');
    assert.equal(rule.cssText, `.bar {
  border: 1px solid #000;
  color: green !important;
}`);
  });

  test('style getter', () => {
    let rule = styleRule();
    assert(rule.style instanceof CSSStyleDeclaration);
    assert.equal(rule.style, rule.style);
  });

  test('style setter', () => {
    let rule = styleRule();
    rule.style = 'background: red';
    assert.equal(rule.cssText, `.foo {
  background: red;
}`);
  });
});

run('CSSStyleDeclaration', test => {
  test('cssText', () => {
    assert.equal(declaration().cssText, 'border: 1px solid #000; color: green !important');
  });

  test('parentRule', () => {
    let rule = styleRule();
    assert.equal(rule.style.parentRule, rule);
  });

  test('length', () => {
    assert.equal(declaration().length, 13);
  });

  test('item', () => {
    let style = declaration();
    assert.equal(style.item(0), 'border-top-width');
    assert.equal(style.item(12), 'color');
  });

  test('getPropertyValue', () => {
    let style = declaration();
    assert.equal(style.getPropertyValue('border'), '1px solid #000');
    assert.equal(style.getPropertyValue('border-color'), '#000');
    assert.equal(style.getPropertyValue('border-top'), '1px solid #000');
    assert.equal(style.getPropertyValue('border-top-style'), 'solid');
    assert.equal(style.getPropertyValue('color'), 'green');
  });

  test('getPropertyPriority', () => {
    let style = declaration();
    assert.equal(style.getPropertyPriority('border'), '');
    assert.equal(style.getPropertyPriority('border-top'), '');
    assert.equal(style.getPropertyPriority('color'), 'important');
  });

  test('setProperty', () => {
    let style = declaration();
    style.setProperty('border-top-color', 'red');
    assert.equal(style.cssText, 'border: 1px solid #000; border-top-color: red; color: green !important');
    style.setProperty('border-top-color', 'green', 'important');
    assert.equal(style.cssText, 'border: 1px solid #000; color: green !important; border-top-color: green !important');
    style.setProperty('color', 'green');
    assert.equal(style.cssText, 'border: 1px solid #000; color: green; border-top-color: green !important');
  });

  test('removeProperty', () => {
    let style = declaration();
    style.removeProperty('color');
    assert.equal(style.cssText, 'border: 1px solid #000');
    style.removeProperty('border-top');
    assert.equal(style.cssText, 'border-bottom: 1px solid #000; border-left: 1px solid #000; border-right: 1px solid #000');
  });
});
