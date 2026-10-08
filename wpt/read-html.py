"""Read HTML scripts and metadata without adding a JavaScript HTML dependency."""
import json
import sys
from html.parser import HTMLParser


class Document(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=False)
        self.scripts = []
        self.current = None
        self.variants = []
        self.context_free = True

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == 'script':
            self.current = {'attrs': attrs, 'code': '', 'line': self.getpos()[0]}
        if tag == 'style' or tag == 'base' or 'style' in attrs or attrs.get('id') == 'target':
            self.context_free = False
        if tag == 'link' and attrs.get('rel') == 'stylesheet':
            self.context_free = False
        if tag == 'meta' and attrs.get('name') == 'variant':
            self.variants.append(attrs.get('content', ''))

    def handle_data(self, data):
        if self.current is not None:
            self.current['code'] += data

    def handle_endtag(self, tag):
        if tag == 'script' and self.current is not None:
            self.scripts.append(self.current)
            self.current = None


for line in sys.stdin:
    source = json.loads(line)
    parser = Document()
    parser.feed(source)
    print(json.dumps({'scripts': parser.scripts, 'variants': parser.variants,
                      'contextFree': parser.context_free}))
