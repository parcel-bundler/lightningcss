"""Read HTML scripts and metadata without adding a JavaScript HTML dependency."""
import json
import sys
from html.parser import HTMLParser


class Document(HTMLParser):
    # With scripting enabled, noscript contents are text, not executable markup.
    CDATA_CONTENT_ELEMENTS = HTMLParser.CDATA_CONTENT_ELEMENTS + ('noscript',)

    def __init__(self):
        super().__init__(convert_charrefs=False)
        self.scripts = []
        self.current = None
        self.variants = []
        self.context_free = True
        self.template_depth = 0
        self.unsupported_reason = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == 'template':
            if not self.template_depth and 'shadowrootmode' in attrs:
                self.unsupported_reason = 'declarative shadow roots require a browser adapter'
            self.template_depth += 1
        if self.template_depth:
            return
        if tag == 'script':
            self.current = {'attrs': attrs, 'code': '', 'line': self.getpos()[0]}
        if tag == 'style' or tag == 'base' or 'style' in attrs or attrs.get('id') == 'target':
            self.context_free = False
        if tag == 'link' and 'stylesheet' in (attrs.get('rel') or '').lower().split():
            self.context_free = False
        if any(name.startswith('on') for name in attrs):
            self.context_free = False
        if tag == 'meta' and attrs.get('name') == 'variant':
            self.variants.append(attrs.get('content', ''))

    def handle_data(self, data):
        if self.current is not None:
            self.current['code'] += data

    def handle_endtag(self, tag):
        if tag == 'template' and self.template_depth:
            self.template_depth -= 1
            return
        if self.template_depth:
            return
        if tag == 'script' and self.current is not None:
            self.scripts.append(self.current)
            self.current = None


for line in sys.stdin:
    source = json.loads(line)
    parser = Document()
    parser.feed(source)
    result = {'scripts': parser.scripts, 'variants': parser.variants,
              'contextFree': parser.context_free}
    if parser.unsupported_reason:
        result['unsupportedReason'] = parser.unsupported_reason
    print(json.dumps(result))
