"""Independently check department scoring arithmetic and the self-contained HTML."""
from pathlib import Path
import argparse,base64,hashlib,json,math,re,statistics,xml.etree.ElementTree as ET
from html.parser import HTMLParser

ap=argparse.ArgumentParser();ap.add_argument('--data-dir',required=True);ap.add_argument('--html',required=True)
a=ap.parse_args();root=Path(a.data_dir);report=Path(a.html)
p=json.loads((root/'report_data.json').read_text());text=report.read_text()
D,S=p['demand_variables'],p['supply_variables'];areas=sorted({r['geo'] for r in p['rates']})
hm={(r['geo'],r['side'],r['variable']):r['value'] for r in p['heatmap']}
saved={(r['method'],r['geo']):r for r in p['scores']}
def quantile(v,q):
    v=sorted(v);i=(len(v)-1)*q;lo=math.floor(i);return v[lo]+(v[math.ceil(i)]-v[lo])*(i-lo)
def rank(v,x):
    if x is None:return None
    return 1+sum(z<x for z in v)+(sum(z==x for z in v)-1)/2
mat={};zcomp={}
for side,fields in [('Demand',D),('Supply',S)]:
    for k in fields:
        values=[hm[g,side,k] for g in areas if hm[g,side,k] is not None]
        mu=statistics.mean(values);sd=statistics.stdev(values);med=statistics.median(values);iqr=quantile(values,.75)-quantile(values,.25)
        for g in areas:
            x=hm[g,side,k]
            for method,value in [('raw',x),('rank',rank(values,x)),('robust',None if x is None else (x-med)/iqr),('zscore',None if x is None else (x-mu)/sd),('median_count',None if x is None else int(x>med+1e-10) if side=='Demand' else int(x<med-1e-10))]:
                mat[g,side,k,method]=value
            zcomp[g,side,k]=mat[g,side,k,'zscore']
score_checks=0
for method in ['rank','raw','robust','zscore','median_count']:
    scores={}
    for side,fields in [('Demand',D),('Supply',S)]:
        for g in areas:
            values=[mat[g,side,k,method] for k in fields]
            scores[g,side]=None if any(v is None for v in values) else sum(values)/(1 if method=='median_count' else 6)
    dc=4 if method=='median_count' else statistics.median(v for (g,s),v in scores.items() if s=='Demand' and v is not None)
    sc=4 if method=='median_count' else statistics.median(v for (g,s),v in scores.items() if s=='Supply' and v is not None)
    for g in areas:
        ds,ss=scores[g,'Demand'],scores[g,'Supply'];q=saved[method,g]
        for v,field in [(ds,'demand_score'),(ss,'supply_score')]:
            assert (v is None and q[field] is None) or (v is not None and q[field] is not None and abs(v-q[field])<1e-8),(method,g,field)
        df=None if ds is None else ds>=dc if method=='median_count' else ds>dc+1e-10
        sf=None if ss is None else ss>=sc if method=='median_count' else ss<sc-1e-10
        cat='unassessed' if df is None or sf is None else 'both' if df and sf else 'demand' if df else 'supply' if sf else 'neither'
        assert (q['demand_flag'],q['supply_flag'],q['category'])==(df,sf,cat),(method,g)
        score_checks+=1
for r in p['planning']:
    g=r['geo'];q=saved['zscore',g];assert r['category']==q['category']
    demand=sorted(D,key=lambda k:(-zcomp[g,'Demand',k],D.index(k)))[:3]
    assert r['top_demand']==demand
    if q['supply_score'] is not None:
        supply=sorted(S,key=lambda k:(zcomp[g,'Supply',k],S.index(k)))[:3]
        assert r['top_supply']==supply
class Parser(HTMLParser):
    def __init__(self):super().__init__();self.ids=[];self.anchors=[];self.scripts=[];self.tables=0
    def handle_starttag(self,tag,attrs):
        d=dict(attrs)
        if 'id' in d:self.ids.append(d['id'])
        if tag=='a' and d.get('href','').startswith('#'):self.anchors.append(d['href'][1:])
        if tag=='script' and 'src' in d:self.scripts.append(d['src'])
        if tag=='table':self.tables+=1
h=Parser();h.feed(text)
assert len(h.ids)==len(set(h.ids));assert set(h.anchors)<=set(h.ids);assert not h.scripts
embedded=re.findall(r'data:image/svg\+xml;base64,([^" ]+)',text);assert len(embedded)==10
for v in embedded:ET.fromstring(base64.b64decode(v))
svgs=re.findall(r'<svg[\s\S]*?</svg>',text);assert len(svgs)==3
for svg in svgs:ET.fromstring(svg)
data=json.loads(re.search(r'const DATA=(\{[\s\S]*?\});const tip=',text).group(1))
assert set(data)=={r['geo'] for r in p['coverage'] if r['geo']!='SENEGAL'}
for g in areas:
    q=saved['zscore',g]
    assert data[g]['demand']==q['demand_score'] and data[g]['supply']==q['supply_score']
    assert data[g]['small_sample']==(data[g]['n']<50)
    expected_coverage='both' if data[g]['women']>0 and data[g]['facilities']>0 else 'demand_only' if data[g]['women']>0 else 'supply_only' if data[g]['facilities']>0 else 'neither'
    assert data[g]['coverage_group']==expected_coverage
assert sum(d['coverage_group']=='both' for d in data.values())==33
assert sum(d['coverage_group']=='supply_only' for d in data.values())==13
assert all(d['cpr'] is None and d['mcpr'] is None for d in data.values() if d['coverage_group']=='supply_only')
assert sum(d['small_sample'] for d in data.values())==sum(r['n']<50 for r in p['rates'])
assert 'Mbacke' in text and 'health-district lookup' in text and '522' in text
assert 'How the scores and map thresholds are constructed' in text
assert all(r['denominator']==1905 for r in p['bars'])
assert sum(r['n'] for r in p['bars'] if r['field'] in ['user','intender','no_intent','unresolved'])==1905
assert 'original SURGO' in text and 'FPN2 = 1' in text
assert 'need_n recoding' in text
assert 'modern contraceptive use' in text.lower()
assert 'Rural/Urban residence' in text
private_prefix=chr(47)+'Us'+'ers'+chr(47)
trace_pattern='(?i:'+'co'+'dex|'+'chat'+'gpt|'+'open'+'ai)'
for pattern in [private_prefix,r'[A-Za-z]:[\\/](?:Users|Desktop|Documents)',trace_pattern]:
    assert not re.search(pattern,text),pattern
source=Path(__file__).resolve().parents[1]/'analysis/senegal'
for f in source.iterdir():
    if f.is_file() and f.suffix in ['.R','.py','.md','.css']:
        assert not re.search(private_prefix+'|'+trace_pattern,f.read_text()),f.name
check={'independent_department_method_rows':score_checks,'all_score_values_flags_categories_reproduced':True,
       'planning_priorities_match_component_zscores':True,'map_values_match_scores':True,
       'embedded_model_PCA_figures':len(embedded),'coverage_bar_classification_SVGs':len(svgs),
       'unique_HTML_ids':True,'navigation_targets_valid':True,'self_contained_images_and_no_remote_scripts':True,
       'all_overview_bar_denominators':1905,'private_paths_and_assistant_traces_absent':True,
       'small_need_group_threshold':50,'small_need_group_departments_flagged':sum(r['n']<50 for r in p['rates']),
       'coverage_department_counts':{'both':33,'demand_only':0,'supply_only':13},
       'html_sha256':hashlib.sha256(report.read_bytes()).hexdigest()}
(root/'checks/report_verification.json').write_text(json.dumps(check,indent=2))
(root/'checks/report_script.js').write_text(re.findall(r'<script>([\s\S]*?)</script>',text)[-1])
print('Verified all 165 method/department rows, map and planning consistency, self-contained figures, and HTML structure.')
