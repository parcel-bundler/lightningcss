const { suite } = require('uvu');
const { CSSStyleSheet, CSSStyleRule, CSSRuleList, CSSRule, CSSGroupingRule, CSSConditionRule, CSSMediaRule, CSSStyleDeclaration, MediaList, CSSSupportsRule, CSSKeyframesRule, CSSKeyframeRule, CSSImportRule, CSSNamespaceRule, CSSLayerStatementRule, CSSLayerBlockRule, CSSPageRule } = require('../');
const assert = require('assert');

Object.setPrototypeOf(CSSStyleRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSStyleRule, CSSRule);

Object.setPrototypeOf(CSSGroupingRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSGroupingRule, CSSRule);

Object.setPrototypeOf(CSSConditionRule.prototype, CSSGroupingRule.prototype);
Object.setPrototypeOf(CSSConditionRule, CSSGroupingRule);

Object.setPrototypeOf(CSSMediaRule.prototype, CSSConditionRule.prototype);
Object.setPrototypeOf(CSSMediaRule, CSSConditionRule);

Object.setPrototypeOf(CSSSupportsRule.prototype, CSSConditionRule.prototype);
Object.setPrototypeOf(CSSSupportsRule, CSSConditionRule);

Object.setPrototypeOf(CSSKeyframesRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSKeyframesRule, CSSRule);

Object.setPrototypeOf(CSSKeyframeRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSKeyframeRule, CSSRule);

Object.setPrototypeOf(CSSImportRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSImportRule, CSSRule);

Object.setPrototypeOf(CSSNamespaceRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSNamespaceRule, CSSRule);

Object.setPrototypeOf(CSSLayerStatementRule.prototype, CSSRule.prototype);
Object.setPrototypeOf(CSSLayerStatementRule, CSSRule);

Object.setPrototypeOf(CSSLayerBlockRule.prototype, CSSGroupingRule.prototype);
Object.setPrototypeOf(CSSLayerBlockRule, CSSGroupingRule);

Object.setPrototypeOf(CSSPageRule.prototype, CSSGroupingRule.prototype);
Object.setPrototypeOf(CSSPageRule, CSSGroupingRule);

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

@supports (display: flex) {
  .baz {
    display: flex;
  }
}

@keyframes test {
  from {
    opacity: 0;
  }

  25%, 75% {
    opacity: 0.7;
  }

  to {
    opacity: 1;
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

function supportsRule() {
  return createStyleSheet().cssRules.item(2);
}

function keyframesRule() {
  return createStyleSheet().cssRules.item(3);
}

function keyframeRule() {
  return createStyleSheet().cssRules.item(3).cssRules.item(0);
}

function declaration() {
  return styleRule().style;
}

function mediaList() {
  return mediaRule().media;
}

// global.gc();
// console.log(process.memoryUsage());

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
    assert.equal(stylesheet.cssRules.length, 5);
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
    assert.equal(stylesheet.cssRules.length, 6);
    assert.equal(stylesheet.cssRules.item(0), newRule);
    assert.equal(stylesheet.cssRules.item(2), rule);

    assert.throws(() => stylesheet.insertRule('.bar { color: red }', 10));
    assert.throws(() => stylesheet.insertRule('.bar { color: red }', -1));
  });

  test('addRule', () => {
    let stylesheet = createStyleSheet();
    stylesheet.addRule('.bar', 'color: red');
    assert.equal(stylesheet.cssRules.length, 5);
    let newRule = stylesheet.cssRules.item(0);
    assert.equal(newRule.cssText, `.bar {
  color: red;
}`);
  });

  test('deleteRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    let style = rule.style;
    stylesheet.deleteRule(0);
    assert.equal(stylesheet.cssRules.length, 3);
    assert.equal(rule.parentStyleSheet, null);
    assert.equal(rule.cssText, `.foo {
  border: 1px solid #000;
  color: green !important;
}`);
    assert.equal(style.getPropertyValue('color'), 'green');

    rule = stylesheet.cssRules.item(0);
    assert(rule instanceof CSSMediaRule);
    let rules = rule.cssRules;
    let media = rule.media;
    stylesheet.deleteRule(0);
    assert(rules.item(0) instanceof CSSStyleRule);
    assert.equal(rules.item(0).style.cssText, 'font-family: Helvetica');
    assert.equal(media.item(0), 'print');

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

    let newRule = rules.item(0);
    assert.equal(newRule.cssText, `.bar {
  color: red;
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
    assert.equal(stylesheet.cssRules.length, 4);
  });

  test('item() returns correct rule subclasses', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(0);
    assert(rule instanceof CSSStyleRule);
    assert(rule instanceof CSSRule);

    rule = stylesheet.cssRules.item(1);
    assert(rule instanceof CSSMediaRule);
    assert(rule instanceof CSSRule);

    rule = stylesheet.cssRules.item(2);
    assert(rule instanceof CSSSupportsRule);
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
    assert.equal(mediaRule().type, 4);
    assert.equal(keyframesRule().type, 7);
    assert.equal(supportsRule().type, 12);
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

  test('nesting', () => {
    let rule = styleRule();
    assert(rule.cssRules instanceof CSSRuleList);
    assert.equal(rule.cssRules.length, 0);

    // TODO
    // rule.insertRule('&.bar { color: red }');
    // assert.equal(rule.cssRules.length, 1);
    // assert.equal(rule.cssRules.item(0).selectorText, '&.bar');
  });
});

run('CSSStyleDeclaration', test => {
  test('cssText', () => {
    assert.equal(declaration().cssText, 'border: 1px solid #000; color: green !important');
  });

  test('set cssText', () => {
    let decl = declaration();
    decl.cssText = 'color: purple';
    assert.equal(decl.cssText, 'color: purple');
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

  test('set conditionText', () => {
    let rule = mediaRule();
    rule.conditionText = 'screen';
    assert.equal(rule.conditionText, 'screen');
    assert.equal(rule.cssText, `@media screen {
  .bar {
    font-family: Helvetica;
  }
}`);
  });

  test('has a media list', () => {
    let rule = mediaRule();
    assert(rule.media instanceof MediaList);
  });

  test('set media', () => {
    let rule = mediaRule();
    rule.media = 'screen';
    assert.equal(rule.conditionText, 'screen');
    assert.equal(rule.cssText, `@media screen {
  .bar {
    font-family: Helvetica;
  }
}`);
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

  test('deleteRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(1);
    let child = rule.cssRules.item(0);
    rule.deleteRule(0);
    assert.equal(rule.cssRules.length, 0);
    assert.equal(child.parentStyleSheet, null);
    assert.equal(child.parentRule, null);
    assert.equal(child.cssText, `.bar {
  font-family: Helvetica;
}`);
    assert.equal(rule.cssText, `@media print, screen and (min-width: 240px) {
  
}`);

    assert.throws(() => rule.deleteRule(10));
    assert.throws(() => rule.deleteRule(-1));
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

  test('set mediaText', () => {
    let media = mediaList();
    media.mediaText = 'screen';
    assert.equal(media.mediaText, 'screen');
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

run('CSSSupportsRule', test => {
  test('has correct prototype chain', () => {
    let rule = supportsRule();
    assert(rule instanceof CSSSupportsRule);
    assert(rule instanceof CSSConditionRule);
    assert(rule instanceof CSSGroupingRule);
    assert(rule instanceof CSSRule);
  });

  test('conditionText', () => {
    let rule = supportsRule();
    assert.equal(rule.conditionText, '(display: flex)');
  });

  test('set conditionText', () => {
    let rule = supportsRule();
    rule.conditionText = '(display: grid)';
    // It does nothing according to the spec...
    assert.equal(rule.conditionText, '(display: flex)');
  });

  test('insertRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(2);
    let child = rule.cssRules.item(0);
    let index = rule.insertRule('.qux { color: red }');
    assert.equal(index, 0);
    assert.equal(rule.cssRules.length, 2);
    assert.equal(rule.cssRules.item(1), child);
    let newRule = rule.cssRules.item(0);
    assert(newRule instanceof CSSStyleRule);
    assert.equal(newRule.parentStyleSheet, stylesheet);
    assert.equal(newRule.parentRule, rule);
    assert.equal(newRule.cssText, `.qux {
  color: red;
}`);
    assert.equal(child.cssText, `.baz {
  display: flex;
}`);

    rule.insertRule('.x { color: green }', 1);
    assert.equal(rule.cssRules.length, 3);
    assert.equal(rule.cssRules.item(0), newRule);
    assert.equal(rule.cssRules.item(2), child);

    assert.throws(() => rule.insertRule('.bar { color: red }', 10));
    assert.throws(() => rule.insertRule('.bar { color: red }', -1));

    assert.equal(rule.cssText, `@supports (display: flex) {
  .qux {
    color: red;
  }

  .x {
    color: green;
  }

  .baz {
    display: flex;
  }
}`)
  });

  test('deleteRule', () => {
    let stylesheet = createStyleSheet();
    let rule = stylesheet.cssRules.item(2);
    let child = rule.cssRules.item(0);
    rule.deleteRule(0);
    assert.equal(rule.cssRules.length, 0);
    assert.equal(child.parentStyleSheet, null);
    assert.equal(child.cssText, `.baz {
  display: flex;
}`);
    assert.equal(rule.cssText, `@supports (display: flex) {
  
}`);

    assert.throws(() => rule.deleteRule(10));
    assert.throws(() => rule.deleteRule(-1));
  });
});

run('CSSKeyframesRule', test => {
  test('has correct prototype chain', () => {
    let rule = keyframesRule();
    assert(rule instanceof CSSKeyframesRule);
    assert(rule instanceof CSSRule);
  });

  test('cssText', () => {
    let rule = keyframesRule();
    assert.equal(rule.cssText, `@keyframes test {
  from {
    opacity: 0;
  }

  25%, 75% {
    opacity: .7;
  }

  to {
    opacity: 1;
  }
}`);
  });

  test('name', () => {
    let rule = keyframesRule();
    assert.equal(rule.name, 'test');
  });

  test('set name', () => {
    let rule = keyframesRule();
    rule.name = 'hi';
    assert.equal(rule.name, 'hi');
    assert.equal(rule.cssText, `@keyframes hi {
  from {
    opacity: 0;
  }

  25%, 75% {
    opacity: .7;
  }

  to {
    opacity: 1;
  }
}`);
  });

  test('cssRules', () => {
    let rule = keyframesRule();
    assert(rule.cssRules instanceof CSSRuleList);
    assert.equal(rule.cssRules.length, 3);

    let child = rule.cssRules.item(0);
    assert(child instanceof CSSKeyframeRule);
    assert.equal(child.type, 8);
    assert.equal(child.parentRule, rule);
  });

  test('findRule', () => {
    let rule = keyframesRule();
    let child = rule.findRule('from');
    assert(child instanceof CSSKeyframeRule);
    assert(child.keyText, 'from');
    assert.equal(child, rule.cssRules.item(0));

    child = rule.findRule('0%');
    assert(child.keyText, 'from');
    assert.equal(child, rule.cssRules.item(0));

    child = rule.findRule('to');
    assert.equal(child.keyText, 'to');
    assert.equal(child, rule.cssRules.item(2));

    child = rule.findRule('100%');
    assert.equal(child, rule.cssRules.item(2));

    child = rule.findRule('25%, 75%');
    assert.equal(child.keyText, '25%, 75%');
    assert.equal(child, rule.cssRules.item(1));

    child = rule.findRule('25%,75%');
    assert.equal(child, rule.cssRules.item(1));

    assert.equal(rule.findRule('25%'), null);
    assert.equal(rule.findRule('25%, 75%, 80%'), null);
    assert.equal(rule.findRule('15%'), null);
    assert.equal(rule.findRule('invalid'), null);
  });

  test('appendRule', () => {
    let rule = keyframesRule();
    rule.appendRule('80% { color: teal }');
    assert.equal(rule.cssRules.length, 4);

    let child = rule.cssRules.item(3);
    assert.equal(child.keyText, '80%');
    assert.equal(child.cssText, `80% {
  color: teal;
}`);

    assert.equal(rule.findRule('80%'), child);

    rule.appendRule('80% { color: brown }');
    assert.equal(rule.cssRules.length, 5);
    assert.equal(rule.findRule('80%'), rule.cssRules.item(4));
  });

  test('deleteRule', () => {
    let rule = keyframesRule();
    let first = rule.cssRules.item(0);
    rule.deleteRule('from');
    assert.equal(rule.cssRules.length, 2);
    assert.notEqual(rule.cssRules.item(0), first);
    assert.equal(rule.cssRules.item(0).keyText, '25%, 75%');
    assert.equal(rule.cssRules.item(1).keyText, 'to');
    assert.equal(first.keyText, 'from');
    assert.equal(first.parentRule, null);
    assert.equal(first.parentStyleSheet, null);
  });
});

run('CSSKeyframeRule', test => {
  test('keyText', () => {
    let rule = keyframeRule();
    assert.equal(rule.keyText, 'from');
  });

  test('cssText', () => {
    let rule = keyframeRule();
    assert.equal(rule.cssText, `from {
  opacity: 0;
}`);
  });

  test('set keyText', () => {
    let rule = keyframeRule();
    rule.keyText = '23%';
    assert.equal(rule.keyText, '23%');
    assert.equal(rule.cssText, `23% {
  opacity: 0;
}`);
  });

  test('style', () => {
    let rule = keyframeRule();
    assert(rule.style instanceof CSSStyleDeclaration);
    assert.equal(rule.style.getPropertyValue('opacity'), '0');
    assert.equal(rule.style.cssText, 'opacity: 0');

    rule.style.setProperty('opacity', '0.5');
    assert.equal(rule.style.cssText, 'opacity: .5');
    assert.equal(rule.cssText, `from {
  opacity: .5;
}`);
  });

  test('set style', () => {
    let rule = keyframeRule();
    rule.style = 'opacity: 0.5';
    assert.equal(rule.cssText, `from {
  opacity: .5;
}`);
  });
});

run('CSSImportRule', test => {
  let stylesheet = new CSSStyleSheet();
  stylesheet.replaceSync(`
    @import "a.css";
    @import "b.css" print;
    @import "c.css" layer(foo.bar);
    @import "d.css" layer;
  `);

  test('has correct prototype chain', () => {
    let rule = stylesheet.cssRules.item(0);
    assert(rule instanceof CSSImportRule);
    assert(rule instanceof CSSRule);
  })

  test('href', () => {
    assert.equal(stylesheet.cssRules.item(0).href, 'a.css');
  });

  test('media', () => {
    let media = stylesheet.cssRules.item(0).media;
    assert(media instanceof MediaList);
    assert.equal(media.length, 0);

    media = stylesheet.cssRules.item(1).media;
    assert.equal(media.length, 1);
    assert.equal(media.item(0), 'print');
  });

  test('set media', () => {
    let rule = stylesheet.cssRules.item(1);
    rule.media = 'screen';
    let media = rule.media;
    assert.equal(media.length, 1);
    assert.equal(media.item(0), 'screen');
    assert.equal(rule.cssText, '@import "b.css" screen;');
  });

  test('layerName', () => {
    assert.equal(stylesheet.cssRules.item(0).layerName, null);
    assert.equal(stylesheet.cssRules.item(1).layerName, null);
    assert.equal(stylesheet.cssRules.item(2).layerName, 'foo.bar');
    assert.equal(stylesheet.cssRules.item(3).layerName, '');
  });
});

run('CSSNamespaceRule', test => {
  let stylesheet = new CSSStyleSheet();
  stylesheet.replaceSync(`
    @namespace "http://toto.example.org";
    @namespace toto "http://toto.example.org";
  `);

  test('namespaceURI', () => {
    assert.equal(stylesheet.cssRules.item(0).namespaceURI, 'http://toto.example.org');
    assert.equal(stylesheet.cssRules.item(1).namespaceURI, 'http://toto.example.org');
  });

  test('prefix', () => {
    assert.equal(stylesheet.cssRules.item(0).prefix, '');
    assert.equal(stylesheet.cssRules.item(1).prefix, 'toto');
  });
});

run('CSSLayerStatementRule', test => {
  let stylesheet = new CSSStyleSheet();
  stylesheet.replaceSync(`
    @layer foo, bar.baz;
  `);

  test('has correct prototype chain', () => {
    let rule = stylesheet.cssRules.item(0);
    assert(rule instanceof CSSLayerStatementRule);
    assert(rule instanceof CSSRule);
  });

  test('nameList', () => {
    assert.deepEqual(stylesheet.cssRules.item(0).nameList, ['foo', 'bar.baz']);
  });
});

run('CSSLayerBlockRule', test => {
  let stylesheet = new CSSStyleSheet();
  stylesheet.replaceSync(`
    @layer outer {
      @layer foo.bar {
        .a {
          color: red;
        }
      }

      .b {
        color: green;
      }
    }
  `);

  test('has correct prototype chain', () => {
    let rule = stylesheet.cssRules.item(0);
    assert(rule instanceof CSSLayerBlockRule);
    assert(rule instanceof CSSGroupingRule);
    assert(rule instanceof CSSRule);
  });

  test('name', () => {
    let rule = stylesheet.cssRules.item(0);
    assert.equal(rule.name, 'outer');

    rule = rule.cssRules.item(0);
    assert.equal(rule.name, 'foo.bar');
  });

  test('cssRules', () => {
    let rule = stylesheet.cssRules.item(0);
    assert(rule.cssRules instanceof CSSRuleList);
    assert.equal(rule.cssRules.length, 2);
    assert(rule.cssRules.item(0) instanceof CSSLayerBlockRule);
    assert(rule.cssRules.item(1) instanceof CSSStyleRule);
    assert.equal(rule.cssRules.item(1).style.getPropertyValue('color'), 'green');

    rule = rule.cssRules.item(0);
    assert(rule.cssRules instanceof CSSRuleList);
    assert.equal(rule.cssRules.length, 1);
    assert(rule.cssRules.item(0) instanceof CSSStyleRule);
    assert.equal(rule.cssRules.item(0).style.getPropertyValue('color'), 'red');
  });
});

run('CSSPageRule', test => {
  function rules() {
    let stylesheet = new CSSStyleSheet();
    stylesheet.replaceSync(`
      @page {
        margin: 0.5cm;
      }

      @page :first {
        margin: 1in;
      }
    `);
    return stylesheet.cssRules;
  }

  test('has correct prototype chain', () => {
    let rule = rules().item(0);
    assert(rule instanceof CSSPageRule);
    assert(rule instanceof CSSGroupingRule);
    assert(rule instanceof CSSRule);
  });

  test('selectorText', () => {
    let cssRules = rules();
    let rule = cssRules.item(0);
    assert.equal(rule.selectorText, '');

    rule = cssRules.item(1);
    assert.equal(rule.selectorText, ':first');
  });

  test('set selectorText', () => {
    let rule = rules().item(0);
    rule.selectorText = ':last';
    assert.equal(rule.selectorText, ':last');
    assert.equal(rule.cssText, `@page :last {
  margin: .5cm;
}`);
  });

  test('style', () => {
    let rule = rules().item(0);
    assert(rule.style instanceof CSSStyleDeclaration);
    assert.equal(rule.style.getPropertyValue('margin'), '.5cm');

    rule.style.setProperty('margin', '1in');
    assert.equal(rule.cssText, `@page {
  margin: 1in;
}`);
  });

  test('set style', () => {
    let rule = rules().item(0);
    rule.style = 'margin: 2in';
    assert.equal(rule.cssText, `@page {
  margin: 2in;
}`)
  });
});

// setTimeout(() => {
//   console.log("GC")
//   global.gc();
//   setTimeout(() => {
//     console.log(process.memoryUsage());
//   }, 500);
// }, 500);