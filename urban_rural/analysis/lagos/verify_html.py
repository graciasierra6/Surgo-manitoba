"""Check the exact LGA-residence report and its embedded aggregate assets."""
import argparse,base64,csv,json,re
from pathlib import Path
from html.parser import HTMLParser
ap=argparse.ArgumentParser();ap.add_argument('--html',required=True);ap.add_argument('--data-dir',required=True)
a=ap.parse_args();source=Path(a.html).read_text();root=Path(a.data_dir)
p=json.loads((root/'report_data.json').read_text())
class Inspect(HTMLParser):
 def __init__(self):super().__init__();self.ids=[];self.anchors=[];self.images=[];self.downloads=[];self.maps={};self.text=[]
 def handle_starttag(self,t,attrs):
  d=dict(attrs)
  if 'id' in d:self.ids.append(d['id'])
  if t=='a' and d.get('href','').startswith('#'):self.anchors.append(d['href'][1:])
  if t=='a' and 'download' in d:self.downloads.append(d['href'])
  if t=='img':self.images.append(d.get('src',''))
  if 'lga-region' in d.get('class','') and 'data-rule' in d:self.maps.setdefault(d['data-rule'],[]).append(d['data-key'])
 def handle_data(self,t):self.text.append(t)
f=Inspect();f.feed(source)
assert len(f.ids)==len(set(f.ids)), 'Duplicate IDs'
assert set(f.anchors).issubset(f.ids), 'Broken anchor'
assert {k:len(set(v)) for k,v in f.maps.items()}=={'coverage':20,'zscore':20}
order=['coverage','use','joint','models','definitions','appendix-scoring','appendix-pca']
assert [source.index('<section id="'+i+'"') for i in order]==sorted(source.index('<section id="'+i+'"') for i in order)
assert len(f.images)==10
assert p['audit']['need_n']==2159 and p['audit']['source_rows']==4555
assert p['supply_variables']==['offering','free','training','daily_provision','larc_stock','stockout_burden']
blobs=[source]
for image in f.images:
 assert image.startswith('data:image/svg+xml;base64,')
 blobs.append(base64.b64decode(image.split(',',1)[1]).decode())
for href in f.downloads:
 assert href.startswith('data:text/csv;base64,')
 txt=base64.b64decode(href.split(',',1)[1]).decode();assert list(csv.reader(txt.splitlines()));blobs.append(txt)
for blob in blobs:
 assert not re.search(r'/(?:[Uu]sers|[Hh]ome)/|bearer\s+[a-z0-9]',blob),'Unwanted artifact content'
 assert not any(token in blob.lower() for token in ['co'+'dex','chat'+'gpt','open'+'ai','fi'+'le://']),'Unwanted artifact content'
assert not re.search(r'<(?:script|link|img)[^>]+(?:src|href)=["\']https?://',source,re.I)
text=' '.join(f.text)
for label in ['882','2,159','4,555','PB2A only','Q506','No random effects or interactions']:
 assert label in text,label
z=[r for r in p['thresholds'] if r['method']=='zscore'][0]
assert z['demand_cutoff']==0 and z['supply_cutoff']==0
assert 'demand <b>above zero</b>' in source and 'readiness <b>below zero</b>' in source
assert 'PB2A–C' not in text and 'three_larc_stockin' not in text
assert 'Both-flagged LGAs are included' in text
for key in ['supply-component-summary','demand-component-summary','supply-department-components','demand-department-components']:
 assert key in f.ids
model=json.loads((root/'models_zscore/model_results.json').read_text())
assert model['audit']['districts']==17 and model['audit']['assigned_context_profiles']==18
assert p['audit']['unmatched_need_women']==0 and p['audit']['fallback_need_women']==62 and p['audit']['fallback_source_women']==157
assert 'Ibeju-Lekki' in text and '62' in text and '157' in text
assert 'LGA–Rural/Urban' in text and 'LGA-wide planning' in text
assert 'priority' not in text.lower()
assert str(model['audit']['common_n']) in text.replace(',','')
assert len({r['n'] for r in model['samples']})==1
assert '18 complete assigned supply profiles' in text and 'LGA-wide fallback' in text
print('PASS: exact LGA-residence report, flagged LGA-wide fallback, need-only results, 10 figures and clean embedded assets.')

assert 'appendix-facilities' not in f.ids and 'appendix-demand' not in f.ids
