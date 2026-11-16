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

CURRENT = [
    'reports/Lagos_analysis_aggregate.html', 'reports/Senegal_analysis_aggregate.html',
    'reports/Cote_dIvoire_analysis_aggregate.html', 'reports/Sindh_analysis.html',
    'reports/Lagos_rural_urban_updated.html', 'reports/Senegal_rural_urban_updated.html',
    'reports/CotedIvoire_rural_urban_updated.html', 'reports/Demand_population_classification_check.html',
    'reports/Score_profile_examples.html', 'reports/Score_profile_contrasts.html',
    'reports/Demand_pair_comparisons.html', 'data_checks/Composite score_comparisons.html',
    'reports/Method_awareness_check_Senegal_Lagos.html', 'reports/Data_checks.html',
    'data_checks/Surgo_UoM_checks.html']
HISTORICAL = ['reports/Lagos_analysis.html', 'reports/Senegal_analysis.html',
              'reports/Cote_dIvoire_analysis.html']
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
        super().__init__(); self.skip = []; self.text = []; self.scripts = []; self.images = []; self.csv = []
    def handle_starttag(self, tag, attributes):
        a = dict(attributes)
        if tag in ['script', 'style']: self.skip.append(tag)
        if tag == 'img': self.images.append(a.get('src', ''))
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

if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('--reference-root', required=True)
    ap.add_argument('--rebuilt-root', required=True)
    ap.add_argument('--include-historical', choices=['true','false'], default='true')
    ap.add_argument('--output', required=True)
    args = ap.parse_args()
    reference = Path(args.reference_root); rebuilt = Path(args.rebuilt_root)
    registry_path = reference/'tests/report_reference_hashes.json'
    generated = json.loads(registry_path.read_text()).get('generated_only', {}) if registry_path.is_file() else {}
    files = CURRENT + (HISTORICAL if args.include_historical == 'true' else [])
    records = {}
    for name in files:
        a=reference/name; b=rebuilt/name
        records[name]=compare_report(a, b, generated.get(name, {}).get('sha256'))
        print(name+': '+('PASS' if records[name]['passed'] else 'FAIL'))
    result = {'report_count':len(files), 'all_passed':all(x['passed'] for x in records.values()),
              'byte_identical_count':sum(x.get('byte_identical',False) for x in records.values()),
              'comparison':'Published HTML references: visible text and SVG labels exact; scripts and downloadable values allow 1e-9 absolute / 1e-7 relative floating-point tolerance. Integer strings, identifiers and other text remain exact. Generated-only supplementary outputs without published HTML require exact registered SHA-256. PNG layout is not a numerical test.',
              'reports':records}
    out=Path(args.output);out.parent.mkdir(parents=True,exist_ok=True)
    out.write_text(json.dumps(result,indent=2)+'\n')
    if not result['all_passed']: raise SystemExit('Rebuilt content differs; inspect the validation record before publishing.')
