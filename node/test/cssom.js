const { suite } = require('uvu');
const { CSSStyleSheet, CSSStyleRule, CSSRuleList, CSSRule, CSSGroupingRule, CSSConditionRule, CSSMediaRule, CSSStyleDeclaration, MediaList } = require('../');
const assert = require('assert');

Object.setPrototypeOf(CSSStyleRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSStyleRule, CSSRule);

Object.setPrototypeOf(CSSGroupingRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSGroupingRule, CSSRule);

Object.setPrototypeOf(CSSConditionRule.prototype, CSSGroupingRule.prototype);
Object.setPrototypeOf(CSSConditionRule, CSSGroupingRule);

Object.setPrototypeOf(CSSMediaRule.prototype, CSSConditionRule.prototype);
Object.setPrototypeOf(CSSMediaRule, CSSConditionRule);

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

@media print, screen and (min-width: 240px) {
  .bar {
    font-family: Helvetica;
  }
}
`);
  return stylesheet;
}

function styleRule() {
  return createStyleSheet().cssRules.item(0);
}

function mediaRule() {
  return createStyleSheet().cssRules.item(1);
}

function declaration() {
  return styleRule().style;
}

function mediaList() {
  return mediaRule().media;
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
    assert.equal(stylesheet.cssRules.length, 3);
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
    assert.equal(stylesheet.cssRules.length, 4);
    assert.equal(stylesheet.cssRules.item(0), newRule);
    assert.equal(stylesheet.cssRules.item(2), rule);

    assert.throws(() => stylesheet.insertRule('.bar { color: red }', 10));
    assert.throws(() => stylesheet.insertRule('.bar { color: red }', -1));
  });

  test('addRule', () => {
    let stylesheet = createStyleSheet();
    stylesheet.addRule('.bar', 'color: red');
    assert.equal(stylesheet.cssRules.length, 3);
    let newRule = stylesheet.cssRules.item(0);
    assert.equal(newRule.cssText, `.bar {
  color: red;
}`);
  });

  test('deleteRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    stylesheet.deleteRule(0);
    assert.equal(stylesheet.cssRules.length, 1);
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
    assert.equal(stylesheet.cssRules.length, 2);
  });

  test('item() returns correct rule subclasses', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    assert(rule instanceof CSSStyleRule);
    assert(rule instanceof CSSRule);

    rule = stylesheet.cssRules.item(1);
    assert(rule instanceof CSSMediaRule);
    assert(rule instanceof CSSRule);
  });
});

run('CSSRule', test => {
  test('parentStyleSheet equals stylesheet', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    assert.equal(rule.parentStyleSheet, stylesheet);
  });

  test('parentRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    assert.equal(rule.parentRule, null);

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

run('CSSMediaRule', test => {
  test('has correct prototype chain', () => {
    let rule = mediaRule();
    assert(rule instanceof CSSMediaRule);
    assert(rule instanceof CSSConditionRule);
    assert(rule instanceof CSSGroupingRule);
    assert(rule instanceof CSSRule);
  });

  test('conditionText', () => {
    let rule = mediaRule();
    assert.equal(rule.conditionText, 'print, screen and (min-width: 240px)');
  });

  test('has a media list', () => {
    let rule = mediaRule();
    assert(rule.media instanceof MediaList);
  });

  test('has cssRules', () => {
    let rule = mediaRule();
    assert(rule.cssRules instanceof CSSRuleList);
    assert.equal(rule.cssRules.length, 1);
    let child = rule.cssRules.item(0);
    assert(child instanceof CSSStyleRule);
    assert(child.selectorText, '.bar');
    assert(child.parentRule instanceof CSSMediaRule);
    assert.equal(child.parentRule, rule);
  });

  test('insertRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(1);
    let child = rule.cssRules.item(0);
    let index = rule.insertRule('.baz { color: red }');
    assert.equal(index, 0);
    assert.equal(rule.cssRules.length, 2);
    assert.equal(rule.cssRules.item(1), child);
    let newRule = rule.cssRules.item(0);
    assert(newRule instanceof CSSStyleRule);
    assert.equal(newRule.parentStyleSheet, stylesheet);
    assert.equal(newRule.parentRule, rule);
    assert.equal(newRule.cssText, `.baz {
  color: red;
}`);
    assert.equal(child.cssText, `.bar {
  font-family: Helvetica;
}`);

    rule.insertRule('.qux { color: green }', 1);
    assert.equal(rule.cssRules.length, 3);
    assert.equal(rule.cssRules.item(0), newRule);
    assert.equal(rule.cssRules.item(2), child);

    assert.throws(() => rule.insertRule('.bar { color: red }', 10));
    assert.throws(() => rule.insertRule('.bar { color: red }', -1));

    assert.equal(rule.cssText, `@media print, screen and (min-width: 240px) {
  .baz {
    color: red;
  }

  .qux {
    color: green;
  }

  .bar {
    font-family: Helvetica;
  }
}`)
  });
});

run('MediaList', test => {
  test('length', () => {
    assert.equal(mediaList().length, 2);
  });

  test('item', () => {
    let media = mediaList();
    assert.equal(media.item(0), 'print');
    assert.equal(media.item(1), 'screen and (min-width: 240px)');
    assert.equal(media.item(2), null);
    assert.equal(media.item(-1), null);
  });

  test('mediaText', () => {
    assert.equal(mediaList().mediaText, 'print, screen and (min-width: 240px)');
  });

  test('appendMedium', () => {
    let media = mediaList();
    media.appendMedium('(100px <= height <= 500px)');
    assert.equal(media.length, 3);
    assert.equal(media.item(2), '(100px <= height <= 500px)');
    assert.equal(media.mediaText, 'print, screen and (min-width: 240px), (100px <= height <= 500px)');

    media.appendMedium('print');
    assert.equal(media.mediaText, 'print, screen and (min-width: 240px), (100px <= height <= 500px)');
  });

  test('deleteMedium', () => {
    let media = mediaList();
    media.deleteMedium('screen and (min-width: 240px)');
    assert.equal(media.length, 1);
    assert.equal(media.item(0), 'print');
    assert.equal(media.mediaText, 'print');
    assert.throws(() => media.deleteMedium('screen'));
  });
});
