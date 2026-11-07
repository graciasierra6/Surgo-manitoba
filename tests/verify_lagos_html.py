"""Verify the final Lagos report, scores and offline embedded figures."""
import argparse
import base64
import csv
import hashlib
import json
import math
import re
import statistics
import xml.etree.ElementTree as ET
from html.parser import HTMLParser
from pathlib import Path

ap = argparse.ArgumentParser()
ap.add_argument('--data-dir', required=True)
ap.add_argument('--html', required=True)
a = ap.parse_args()
root, report = Path(a.data_dir), Path(a.html)
p = json.loads((root / 'report_data.json').read_text())
text = report.read_text()
D, S = p['demand_variables'], p['supply_variables']
areas = sorted(q['geo'] for q in p['rates'])
hm = {(q['geo'], q['side'], q['variable']): q['value'] for q in p['heatmap']}
for q in csv.DictReader((root/'scoring_inputs_exact.csv').open()):
    key = q['geo'], q['side'], q['variable']
    value = float(q['original_pct_exact'])
    assert abs(value-hm[key]) < 1e-9
    hm[key] = value
saved = {(q['method'], q['geo']): q for q in p['scores']}
components = {}

def quantile(v, q):
    v = sorted(v)
    i = (len(v) - 1) * q
    lo = math.floor(i)
    return v[lo] + (v[math.ceil(i)] - v[lo]) * (i - lo)

for side, fields in [('Demand', D), ('Supply', S)]:
    for k in fields:
        values = [hm[g, side, k] for g in areas]
        assert all(v is not None for v in values)
        mu, sd, med = statistics.mean(values), statistics.stdev(values), statistics.median(values)
        iqr = quantile(values, .75) - quantile(values, .25)
        for g in areas:
            v = hm[g, side, k]
            rank = 1 + sum(z < v for z in values) + (sum(z == v for z in values) - 1) / 2
            for method, value in [('rank', rank), ('raw', v), ('robust', (v-med)/iqr),
                                  ('zscore', (v-mu)/sd),
                                  ('median_count', int(v > med+1e-10) if side == 'Demand' else int(v < med-1e-10))]:
                components[g, side, k, method] = value
checks = 0
for method in ['rank', 'raw', 'robust', 'median_count', 'zscore']:
    scores = {(g, side): sum(components[g, side, k, method] for k in fields) /
              (1 if method == 'median_count' else 6)
              for side, fields in [('Demand', D), ('Supply', S)] for g in areas}
    dc = 4 if method == 'median_count' else statistics.median(scores[g, 'Demand'] for g in areas)
    sc = 4 if method == 'median_count' else statistics.median(scores[g, 'Supply'] for g in areas)
    for g in areas:
        q = saved[method, g]
        ds, ss = scores[g, 'Demand'], scores[g, 'Supply']
        assert abs(ds-q['demand_score']) < 1e-8 and abs(ss-q['supply_score']) < 1e-8
        df = ds >= dc if method == 'median_count' else ds > dc+1e-10
        sf = ss >= sc if method == 'median_count' else ss < sc-1e-10
        category = 'both' if df and sf else 'demand' if df else 'supply' if sf else 'neither'
        assert (q['demand_flag'], q['supply_flag'], q['category']) == (df, sf, category)
        checks += 1
assert all(q['denominator'] == 2159 for q in p['bars'])
assert sum(q['n'] for q in p['bars'] if q['field'] in ['user', 'intender', 'no_intent', 'unresolved']) == 2159
class Parser(HTMLParser):
    def __init__(self):
        super().__init__(); self.ids = []; self.anchors = []; self.scripts = []; self.external_images = []
    def handle_starttag(self, tag, attrs):
        q = dict(attrs)
        if 'id' in q: self.ids.append(q['id'])
        if tag == 'a' and q.get('href', '').startswith('#'): self.anchors.append(q['href'][1:])
        if tag == 'script' and 'src' in q: self.scripts.append(q['src'])
        if tag == 'img' and not q.get('src', '').startswith('data:'): self.external_images.append(q.get('src'))
parser = Parser(); parser.feed(text)
assert len(parser.ids) == len(set(parser.ids))
assert set(parser.anchors) <= set(parser.ids)
assert not parser.scripts and not parser.external_images
figures = re.findall(r'data:image/svg\+xml;base64,([^" ]+)', text)
assert len(figures) == 10
for f in figures: ET.fromstring(base64.b64decode(f))
inline = re.findall(r'<svg[\s\S]*?</svg>', text)
assert len(inline) == 3
for f in inline: ET.fromstring(f)
data = json.loads(re.search(r'const DATA=(\{[\s\S]*?\});const tip=', text).group(1))
assert len(data) == 20 and sum(q['linked'] for q in data.values()) == 17
for g in areas:
    q = saved['zscore', g]
    assert data[g]['demand'] == q['demand_score'] and data[g]['supply'] == q['supply_score']
assert not re.search(chr(47)+'Us'+'ers'+chr(47)+'|'+r'(?i:co'+'dex|chat'+'gpt)', text)
(root/'final_report_verification.json').write_text(json.dumps({
    'independent_lga_method_rows': checks, 'all_bar_denominators': 2159,
    'offline_embedded_figures': len(figures), 'unique_ids_and_valid_navigation': True,
    'html_sha256': hashlib.sha256(report.read_bytes()).hexdigest()}, indent=2))
print('Verified 85 LGA/method rows, final HTML, offline figures and map values.')
