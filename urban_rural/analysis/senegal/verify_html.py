"""Verify the rendered report, embedded assets and appendix ordering."""
import argparse
import base64
import csv
import json
import re
from html.parser import HTMLParser
from pathlib import Path

ap = argparse.ArgumentParser()
ap.add_argument('--data-dir', required=True)
ap.add_argument('--html', required=True)
a = ap.parse_args()
data = Path(a.data_dir)
source = Path(a.html).read_text(encoding='utf-8')
p = json.loads((data / 'report_data.json').read_text())


class Inspect(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.ids, self.anchors, self.images, self.downloads = [], [], [], []
        self.keys, self.cells, self.tables = {}, [], []
        self.in_cell = False
        self.text = []

    def handle_starttag(self, tag, attrs):
        d = dict(attrs)
        if 'id' in d:
            self.ids.append(d['id'])
        if 'data-mode' in d and tag == 'path':
            self.keys.setdefault(d['data-mode'], []).append(d['data-key'])
        if tag == 'img':
            self.images.append(d['src'])
        if tag == 'a' and d.get('href', '').startswith('#'):
            self.anchors.append(d['href'][1:])
        if tag == 'a' and 'download' in d:
            self.downloads.append(d)
        if tag == 'table':
            self.tables.append([])
        if tag in ('th', 'td'):
            self.in_cell = True
            self.cells = []

    def handle_data(self, text):
        self.text.append(text)
        if self.in_cell:
            self.cells.append(text)

    def handle_endtag(self, tag):
        if tag in ('th', 'td'):
            self.in_cell = False
            self.tables[-1].append(' '.join(self.cells).strip())


parsed = Inspect()
parsed.feed(source)
assert len(parsed.ids) == len(set(parsed.ids)), 'Duplicate element IDs'
assert set(parsed.anchors).issubset(parsed.ids), 'Broken internal links'
order = ['coverage', 'use', 'joint', 'models', 'definitions',
         'appendix-scoring', 'pca']
positions = [source.index(f'<section id="{x}"') for x in order]
assert positions == sorted(positions), 'Report order differs from agreed order'
assert p['supply_variables'] == ['offering','free','training','daily_provision','larc_stock','stockout_burden']
assert p['audit']['source_rows'] == 7169 and p['audit']['need_n'] == 1905
assert p['audit']['fallback_source_women'] == 1815 and p['audit']['fallback_need_women'] == 522 and p['audit']['fallback_departments'] == 16
for mode in ('coverage', 'classification'):
    assert len(set(parsed.keys[mode])) == 46, (mode, 'Missing department boundary')
assert 'colspan="6" class="supply-group"' in source
assert len(parsed.images) == 10, ('Expected model/PCA figures', len(parsed.images))
for src in parsed.images:
    assert src.startswith('data:image/svg+xml;base64,'), 'External or missing figure'
    blob = base64.b64decode(src.split(',', 1)[1]).decode()
    assert '<svg' in blob and '<text' in blob
    assert not re.search(r'\bdepartments?\b',blob,re.I)
for link in parsed.downloads:
    assert link['href'].startswith('data:text/csv;base64,')
    decoded = base64.b64decode(link['href'].split(',', 1)[1]).decode()
    assert list(csv.reader(decoded.splitlines())), 'Empty CSV download'
text = ' '.join(parsed.text)
assert '1,648' in text and '1,568' in text and f'{p["audit"]["need_n"]:,}' in text
model=json.loads((data/'models_zscore/model_results.json').read_text())
assert f'{model["audit"]["common_n"]:,}' in text
assert not re.search(r'\bdepartments?\b',text,re.I), 'Administrative label must be département'
assert 'PB2A only' in text and 's5_6 = 1' in text
assert p['audit']['daily_valid_linked']==1560 and p['audit']['daily_undefined_linked']==8
assert p['audit']['stockout_valid_linked']==1568
z=[r for r in p['scores'] if r['method']=='zscore']
assert all(r['demand_flag']==(r['demand_score']>1e-10) and r['supply_flag']==(r['supply_score'] < -1e-10) for r in z)
assert all(r['demand_cutoff']==0 and r['supply_cutoff']==0 for r in p['thresholds'] if r['method']=='zscore')
for side in ('supply','demand'):
    fragment=source.split(f'<div id="{side}-département-components">',1)[1].split('</table>',1)[0]
    observed=len(re.findall(r'<tr>',fragment))-1
    expected=sum(r[f'{side}_flag'] for r in p['planning'])
    assert observed==expected,(side,observed,expected)
assert 'appendix-facilities' not in parsed.ids and 'appendix-demand' not in parsed.ids
assert 'Daily provision of offered modern methods' in text and 'Stockout burden' in text
assert 'higher = worse' in text and 'stock continuity = 1 − burden' in text
pc=json.loads((data/'pca_appendix/pca_results.json').read_text())
if pc['audit'].get('negative_supply_PC1_loadings'): assert 'mixed signs' in text and 'pattern score' in text
assert 'No random effects or interactions' in text
for pattern in (r'/[Uu]sers/', r'/[Hh]ome/', r'file:[/][/]' ,
                r'(?i)bearer\s+[a-z0-9]'):
    assert not re.search(pattern, source), ('Unwanted artifact content', pattern)
    for src in parsed.images:
        assert not re.search(pattern, base64.b64decode(src.split(',', 1)[1]).decode())
    for link in parsed.downloads:
        assert not re.search(pattern, base64.b64decode(link['href'].split(',', 1)[1]).decode())
assert not re.search(r'<(?:script|link|img)[^>]+(?:src|href)=["\']https?://', source, re.I)
assert not re.search(r'url\(["\']?https?://', source, re.I)
assert model['audit']['supply_distinct_values']==39 and model['audit']['assigned_context_profiles']==49
assert 'health-district lookup' in text and '1,815' in text and '522' in text
assert 'priority' not in text.lower()
print('PASS: report order, six main supply columns, two maps, 10 embedded figures,')
print('aggregate downloads, source counts, stockout direction, updated definitions and portable artifact scan.')
