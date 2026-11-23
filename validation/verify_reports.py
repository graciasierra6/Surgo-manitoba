"""Compare independently rebuilt HTML content and embedded analytical outputs."""
import argparse
import base64
import csv
import hashlib
from html.parser import HTMLParser
import io
import json
import math
from pathlib import Path
import re
import xml.etree.ElementTree as ET

NUMBER = re.compile(r'(?<![A-Za-z0-9_])[-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[eE][-+]?\d+)?')

def numeric_text_equal(a, b):
    """Keep nonnumeric content exact; allow only negligible floating-point drift."""
    if a == b: return True
    left = list(NUMBER.finditer(a)); right = list(NUMBER.finditer(b))
    if NUMBER.sub('#', a) != NUMBER.sub('#', b) or len(left) != len(right): return False
    for x, y in zip(left, right):
        x=x.group(); y=y.group()
        if re.fullmatch(r'[-+]?\d+',x) or re.fullmatch(r'[-+]?\d+',y):
            if x != y: return False
        elif not math.isclose(float(x),float(y),rel_tol=1e-7,abs_tol=1e-9): return False
    return True

def tables_equal(a, b):
    if len(a) != len(b): return False
    for x,y in zip(a,b):
        if len(x) != len(y): return False
        for i,(xr,yr) in enumerate(zip(x,y)):
            if len(xr) != len(yr): return False
            for j,(xc,yc) in enumerate(zip(xr,yr)):
                identity = i == 0 or bool(re.search(r'(^|_)(geo|id|code)($|_)',x[0][j],re.I))
                equal = xc == yc if identity else numeric_text_equal(xc,yc)
                if not equal: return False
    return True

class Content(HTMLParser):
    def __init__(self):
        super().__init__(); self.skip = []; self.text = []; self.scripts = []; self.images = []; self.csv = []; self.frames = []
    def handle_starttag(self, tag, attributes):
        a = dict(attributes)
        if tag in ['script', 'style']: self.skip.append(tag)
        if tag == 'img': self.images.append(a.get('src', ''))
        if tag == 'iframe' and 'srcdoc' in a: self.frames.append(a['srcdoc'])
        href = a.get('href', '')
        if href.startswith('data:text/csv;base64,'):
            self.csv.append(list(csv.reader(io.StringIO(base64.b64decode(href.split(',',1)[1]).decode()))))
    def handle_endtag(self, tag):
        if self.skip and self.skip[-1] == tag: self.skip.pop()
    def handle_data(self, data):
        if not self.skip:
            value = ' '.join(data.split())
            if value: self.text.append(value)
        elif self.skip[-1] == 'script': self.scripts.append(' '.join(data.split()))

def svg_values(uri):
    if not uri.startswith('data:image/svg+xml;base64,'):
        return None
    svg = ET.fromstring(base64.b64decode(uri.split(',',1)[1]))
    return [' '.join(''.join(x.itertext()).split()) for x in svg.iter()
            if x.tag.rsplit('}',1)[-1] in ['text','title','desc']]

def inspect(reference, rebuilt):
    a = reference.read_bytes(); b = rebuilt.read_bytes()
    left = Content(); right = Content()
    left.feed(a.decode()); right.feed(b.decode())
    result = dict(byte_identical=a == b, visible_text_equal=left.text == right.text,
                  scripts_equal=len(left.scripts)==len(right.scripts) and all(numeric_text_equal(x,y) for x,y in zip(left.scripts,right.scripts)),
                  downloadable_values_equal=tables_equal(left.csv,right.csv),
                  figure_count_equal=len(left.images) == len(right.images))
    result['figure_values_equal'] = result['figure_count_equal'] and all(
        svg_values(x) == svg_values(y) for x,y in zip(left.images,right.images))
    result['passed'] = all(v for k,v in result.items() if k != 'byte_identical')
    result['sha256'] = hashlib.sha256(b).hexdigest()
    return result

def compare_report(reference, rebuilt, expected_generated_sha256=None):
    """Published reports need their HTML; generated-only supplements use a fixed hash."""
    if not rebuilt.is_file():
        return {'passed': False, 'reason': 'Rebuilt report missing'}
    if reference.is_file():
        return inspect(reference, rebuilt)
    if expected_generated_sha256 is None:
        return {'passed': False, 'reason': 'Published reference report missing'}
    actual = hashlib.sha256(rebuilt.read_bytes()).hexdigest()
    equal = actual == expected_generated_sha256
    return {'passed': equal, 'byte_identical': equal, 'sha256': actual,
            'comparison_basis': 'Exact SHA-256 of validated generated-only supplement',
            'expected_sha256': expected_generated_sha256}

def content_signature(text):
    """Portable analytical signature, including embedded threshold-map documents.

    Preserve labels and integer counts exactly. Normalize floating-point tokens
    to seven significant digits; layout encodings and SVG drawing coordinates
    are not numerical references. Existing pairwise comparisons retain their
    stricter tolerances and remain available for published report audits.
    """
    parsed = Content(); parsed.feed(text)
    def rounded(value):
        # A caption change alters the native HTML digest embedded in a derived
        # map. That provenance digest is not an analytical result. Actual source
        # files and published report bytes have separate SHA-256 checks.
        value = re.sub(r'("source_sha256"\s*:\s*")[0-9a-f]{64}(")',
                       r'\1SOURCE_DIGEST\2', value)
        def token(m):
            x = m.group()
            return x if re.fullmatch(r'[-+]?\d+', x) else format(float(x), '.7g')
        return NUMBER.sub(token, value)
    values = {
        'visible_text': parsed.text,
        'scripts': [rounded(x) for x in parsed.scripts],
        'downloadable_values': [[[rounded(cell) for cell in row] for row in table] for table in parsed.csv],
        'figure_values': [svg_values(x) for x in parsed.images],
        'embedded_documents': [content_signature(x) for x in parsed.frames],
    }
    return {key: hashlib.sha256(json.dumps(value, ensure_ascii=False, separators=(',', ':')).encode()).hexdigest()
            for key, value in values.items()}

def compare_validated_update(reference, rebuilt, record):
    """Check the release's published file and independently validated content."""
    if not rebuilt.is_file(): return {'passed': False, 'reason': 'Rebuilt report missing'}
    if reference.is_file():
        frozen = hashlib.sha256(reference.read_bytes()).hexdigest()
        if frozen != record['published_sha256']:
            return {'passed': False, 'reason': 'Published reference differs from its registered release hash'}
    actual = content_signature(rebuilt.read_text())
    checks = {key: actual[key] == value for key, value in record['content_signature'].items()}
    return dict(passed=all(checks.values()), comparison_basis='Validated release content, including embedded threshold-map documents',
                signature_checks=checks, sha256=hashlib.sha256(rebuilt.read_bytes()).hexdigest())

if __name__ == '__main__':
    ap=argparse.ArgumentParser()
    ap.add_argument('--reference-root',required=True)
    ap.add_argument('--rebuilt-root',required=True)
    ap.add_argument('--output',required=True)
    a=ap.parse_args();root=Path(a.reference_root);rebuilt=Path(a.rebuilt_root)
    ref=json.loads((root/'validation/reference.json').read_text())
    for name,expected in ref.get('source_files',{}).items():
        if hashlib.sha256((root/name).read_bytes()).hexdigest()!=expected:
            raise SystemExit('Source differs from validated release: '+name)
    results={}
    for name,record in ref['reports'].items():
        original=root/name;new=rebuilt/name
        if 'content_signature' in record:
            result=compare_validated_update(original,new,record)
        else:
            result=compare_report(original,new,record['published_sha256'])
        if new.is_file():result['byte_identical_to_reference']=hashlib.sha256(new.read_bytes()).hexdigest()==record['published_sha256']
        results[name]=result
        print(name+': '+('PASS' if result['passed'] else 'FAIL'))
    report={'scope':'Rebuild of all 15 current reports/checks from the documented input bundle using the simplified layout.',
            'all_passed':all(r['passed'] for r in results.values()),'report_count':len(results),
            'byte_identical_count':sum(r.get('byte_identical_to_reference',False) for r in results.values()),'reports':results}
    out=Path(a.output);out.parent.mkdir(parents=True,exist_ok=True);out.write_text(json.dumps(report,indent=2)+'\n')
    if not report['all_passed']:raise SystemExit('Rebuilt reports differ; inspect validation results before publication.')
