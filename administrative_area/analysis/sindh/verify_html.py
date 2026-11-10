"""Check the z-score-only Sindh report and every embedded aggregate asset."""
import argparse, base64, csv, json, re
from pathlib import Path
from html.parser import HTMLParser

ap=argparse.ArgumentParser()
ap.add_argument('--html',required=True)
ap.add_argument('--data-dir',required=True)
a=ap.parse_args();source=Path(a.html).read_text();root=Path(a.data_dir)
p=json.loads((root/'report_data.json').read_text())
m=json.loads((root/'models_zscore/model_results.json').read_text())

class Inspect(HTMLParser):
    def __init__(self):
        super().__init__();self.ids=[];self.anchors=[];self.images=[];self.downloads=[];self.maps={};self.text=[]
    def handle_starttag(self,tag,attrs):
        d=dict(attrs)
        if 'id' in d:self.ids.append(d['id'])
        if tag=='a' and d.get('href','').startswith('#'):self.anchors.append(d['href'][1:])
        if tag=='a' and 'download' in d:self.downloads.append(d['href'])
        if tag=='img':self.images.append(d.get('src',''))
        if 'data-rule' in d:self.maps.setdefault(d['data-rule'],[]).append(d['data-key'])
    def handle_data(self,data):self.text.append(data)

f=Inspect();f.feed(source);text=' '.join(f.text)
assert len(f.ids)==len(set(f.ids)) and set(f.anchors).issubset(f.ids)
order=['coverage','use','joint','models','definitions']
positions=[source.index('<section id="'+k+'"') for k in order]
assert positions==sorted(positions)
assert {k:len(set(v)) for k,v in f.maps.items()}=={'coverage':30,'zscore':30}
assert len(f.images)==4
assert p['audit']['need_n']==3274 and p['audit']['source_rows']==10364
assert p['supply_variables']==['offering','free','training','daily_provision','larc_stock','stockout_burden']
assert {r['method'] for r in p['scores']}=={'zscore'}
assert len(p['scores'])==30 and p['thresholds'][0]['demand_cutoff']==0 and p['thresholds'][0]['supply_cutoff']==0
assert m['audit']['common_n']==3115 and m['audit']['districts']==30
assert all(r['n']==3115 for r in m['samples'])
for value in ['2,823','3,274','3,115','10,364','Q506','PB2A only','Normalizedweights','clusterCode',
              'No random effects or interactions','278 of 2,823','881','above zero','below zero']:
    assert value in text,value
assert not re.search(r'\b(?:PCA|Lagos|Senegal|rank average|Median/IQR)\b',text,re.I)
assert 'PB2A–C' not in text and 'Three-LARC' not in text
for key in ['supply-component-summary','demand-component-summary','supply-department-components','demand-department-components']:
    assert key in f.ids
blobs=[source]
for src in f.images:
    assert src.startswith('data:image/svg+xml;base64,')
    blobs.append(base64.b64decode(src.split(',',1)[1]).decode())
for href in f.downloads:
    assert href.startswith('data:text/csv;base64,')
    blob=base64.b64decode(href.split(',',1)[1]).decode()
    assert list(csv.reader(blob.splitlines()))
    blobs.append(blob)
for blob in blobs:
    assert not re.search(r'/(?:[Uu]sers|[Hh]ome)/|bearer\s+[a-z0-9]',blob,re.I)
    assert not any(token in blob.lower() for token in ['co'+'dex','chat'+'gpt','open'+'ai','fi'+'le://'])
assert not re.search(r'<(?:script|link|img)[^>]+(?:src|href)=["\']https?://',source,re.I)
print('PASS: z-score only, zero cutoffs, 30-district maps, four model figures, FP restriction, need group, coverage caveat and clean embedded assets.')
