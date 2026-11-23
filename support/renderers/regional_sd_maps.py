"""Build +0.5 final-index SD sensitivity maps from existing aggregate reports.

Each side uses every area with a finite score on that side, preserving the original
study reference: Côte d'Ivoire has 82 demand and 77 supply scores. Unknown joint
categories remain unknown. No survey indicators, supply profiles or models are refit.

Run: python3 analysis/regional_sd_maps.py --threshold 0.5
"""
import argparse
from collections import Counter
from html import escape
import hashlib
import json
import math
from pathlib import Path
import re
import statistics
import xml.etree.ElementTree as ET

from lagos_threshold_comparison import band, classify, table

ET.register_namespace('', 'http://www.w3.org/2000/svg')
ORDER=['Demand only','Supply only','Both','Neither flagged','Supply unassessed','Facility data only','Neither source recorded']
CONFIG=[('cotedivoire','Côte d’Ivoire','Cote_dIvoire_analysis_aggregate.html','départements',82,77,'ABIDJAN'),
        ('senegal','Senegal','Senegal_analysis_aggregate.html','départements',33,33,'GOUDIRY'),
        ('sindh','Sindh','Sindh_analysis.html','districts',30,30,'HYDERABAD')]


def valid(value):
    return isinstance(value,(int,float)) and math.isfinite(value)


def load_country(path,key,title,demand_n,supply_n,threshold):
    html=path.read_text(encoding='utf-8')
    match=re.search(r'const DATA=(\{)',html)
    if not match: raise ValueError('Stored area map data missing')
    start=match.start(1)
    source,_=json.JSONDecoder().raw_decode(html[start:])
    supply_sign=1 if 'data-supply-score-orientation="constraints"' in html else -1
    demand=[r['demand'] for r in source.values() if valid(r.get('demand'))]
    supply=[supply_sign*r['supply'] for r in source.values() if valid(r.get('supply'))]
    assert len(demand)==demand_n and len(supply)==supply_n
    dm=statistics.mean(demand);sm=statistics.mean(supply)
    ds=statistics.stdev(demand);ss=statistics.stdev(supply)
    assert abs(dm)<1e-10 and abs(sm)<1e-10 and ds>0 and ss>0
    rows={}
    for area,r in source.items():
        du=(r['demand']-dm)/ds if valid(r.get('demand')) else None
        su=(supply_sign*r['supply']-sm)/ss if valid(r.get('supply')) else None
        assessed=du is not None and su is not None
        current=r['category'];alternative=current
        if assessed:
            current_d=r['demand']>1e-10;current_s=supply_sign*r['supply']>1e-10
            assert classify(current_d,current_s)==current,(key,area)
            assert classify(du>1e-10,su>1e-10)==current,(key,area,'zero rule changed')
            new_d=du>=threshold;new_s=su>=threshold
            assert not new_d or current_d
            assert not new_s or current_s
            alternative=classify(new_d,new_s)
        rows[area]=dict(name=r['name'],assessed=assessed,current=current,alternative=alternative,
            demand_u=du,supply_u=su,demand_band=band(du),supply_band=band(su),
            n=r.get('n',0),cpr=r.get('cpr'),mcpr=r.get('mcpr'))
    selected_svg=None
    for match in re.finditer(r'<svg\b[\s\S]*?</svg>',html):
        svg=ET.fromstring(match.group())
        paths=[n for n in svg.iter() if n.tag.rsplit('}',1)[-1]=='path' and n.get('data-key')]
        target='zscore' if key=='sindh' else 'classification'
        if any(p.get('data-mode')==target or p.get('data-rule')==target for p in paths):
            selected_svg=svg;break
    assert selected_svg is not None
    palette={};seen=set()
    for p in selected_svg.iter():
        area=p.get('data-key')
        if p.tag.rsplit('}',1)[-1]!='path' or area not in rows:
            continue
        p.set('data-area',area);p.set('tabindex','0');p.set('role','button')
        seen.add(area);category=rows[area]['current'];color=p.get('fill')
        if category in palette:assert palette[category]==color
        palette[category]=color
    assert seen==set(rows),(key,len(seen),len(rows))
    # Prefix all display IDs, including Côte d'Ivoire's unassessed hatch pattern.
    ids={n.get('id'):key+'-'+n.get('id') for n in selected_svg.iter() if n.get('id')}
    def prefixed(value):
        for before,after in ids.items():value=value.replace('url(#'+before+')','url(#'+after+')')
        return value
    for node in selected_svg.iter():
        if node.get('id'):node.set('id',ids[node.get('id')])
        for attr,value in list(node.attrib.items()):
            if attr!='id':node.set(attr,prefixed(value))
    palette={c:prefixed(color) for c,color in palette.items()}
    for node in selected_svg.iter():
        area=node.get('data-area')
        if area:node.set('fill',palette[rows[area]['alternative']])
    selected_svg.set('id','map-'+key)
    selected_svg.set('class',selected_svg.get('class','')+' geo-map')
    selected_svg.set('aria-label',title+': final-index SD threshold sensitivity map')
    changes=[dict(key=k,**r) for k,r in rows.items() if r['assessed'] and r['current']!=r['alternative']]
    info=dict(title=title,areas=rows,palette=palette,assessed=sum(r['assessed'] for r in rows.values()),
        changed=len(changes),current_counts=dict(Counter(r['current'] for r in rows.values())),
        alternative_counts=dict(Counter(r['alternative'] for r in rows.values())),
        reference=dict(demand_n=demand_n,supply_n=supply_n,demand_mean=dm,supply_mean=sm,
            demand_SD=ds,supply_SD=ss,original_demand_cutoff=dm+threshold*ds,original_supply_constraints_cutoff=sm+threshold*ss),
        source_report=path.name,source_sha256=hashlib.sha256(path.read_bytes()).hexdigest())
    return info,ET.tostring(selected_svg,encoding='unicode'),changes


TEMPLATE=r'''<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>SD threshold maps</title>
<style>
:root{--ink:#17374a;--muted:#59717f;--line:#dce6eb;--teal:#047f8b}*{box-sizing:border-box}body{margin:0;background:#f3f6f8;color:var(--ink);font:15px/1.5 Arial,Helvetica,sans-serif}header,main{max-width:1400px;margin:auto;padding:26px 28px}header{padding-bottom:3px}h1{font-size:30px;margin:0 0 8px}h2{font-size:24px;margin:0}h3{font-size:18px;margin:20px 0 8px}p{margin:8px 0}nav{display:flex;gap:20px;flex-wrap:wrap;margin:16px 0}a{color:var(--teal)}section{background:white;border:1px solid var(--line);border-radius:9px;margin-bottom:25px;padding:24px}.head{display:flex;align-items:center;justify-content:space-between;gap:15px;flex-wrap:wrap}.head label{display:flex;align-items:center;gap:8px}.muted{font-size:13px;color:var(--muted)}.controls{display:flex;gap:8px;flex-wrap:wrap;margin:15px 0}button,select{font:inherit;padding:8px 12px;border:1px solid #c4d5de;background:white;color:var(--ink);border-radius:5px}button{cursor:pointer}button[aria-pressed="true"]{background:var(--ink);color:white;border-color:var(--ink)}button:focus-visible,select:focus-visible{outline:3px solid #8cc0c7;outline-offset:2px}.selected{font-weight:600}.scores{display:flex;flex-wrap:wrap;gap:25px;margin:13px 0}.score span{font-size:13px;color:var(--muted);display:block}.score strong{font-size:20px;font-variant-numeric:tabular-nums}.geo-map{width:100%;height:auto;display:block}.geo-map [data-area]{cursor:pointer;vector-effect:non-scaling-stroke;stroke:white;stroke-width:1.1}.geo-map [data-area].selected-shape,.geo-map [data-area]:hover,.geo-map [data-area]:focus{stroke:var(--ink);stroke-width:2;outline:none}.map-label{font-size:11.5px;fill:#254453;font-weight:600;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round}.inset-heading{font-size:16px;fill:#17374a;font-weight:600}.map-caption,.orientation{font-size:12px;fill:var(--muted)}.legend{display:flex;gap:9px 17px;flex-wrap:wrap;margin-top:14px;font-size:13px}.legend span{display:inline-flex;align-items:center;gap:6px}.legend i{width:14px;height:14px;border:1px solid #d5dfe5;display:inline-block}.table-wrap{overflow-x:auto}table{width:100%;border-collapse:collapse;font-size:14px}th,td{border-bottom:1px solid #e4ebef;padding:9px 10px;text-align:left}th{font-weight:600}td.number{text-align:right;font-variant-numeric:tabular-nums}details{margin-top:18px}summary{cursor:pointer;font-weight:600}.tooltip{display:none;position:fixed;z-index:50;pointer-events:none;background:var(--ink);color:white;max-width:300px;padding:12px 15px;border-radius:6px;font-size:13px}.tooltip p{margin:5px 0}.tooltip strong{font-size:15px}.reference{font-size:13px;color:var(--muted)}@media(max-width:650px){header,main{padding:18px 10px}section{padding:15px}.head label{width:100%}.head select{max-width:100%}.map-label{display:none}table{font-size:13px}th,td{padding:8px}}@media print{body{background:white}.controls,.tooltip{display:none}section{break-before:page}section:first-child{break-before:auto}}
</style></head><body><header><h1>SD threshold maps</h1><p>Proposed classification: flag demand or supply constraints at <b>+__T__ SD or higher</b>, using the SD of the final composite score for each side and geography separately. Higher values indicate more challenges.</p><p class="muted">Current maps flag scores above zero. The indicators, survey weights, FP-provider restriction, geographic assignments and original continuous scores are unchanged.</p><nav aria-label="Geographies"><a href="#cotedivoire">Côte d’Ivoire</a><a href="#senegal">Senegal</a><a href="#sindh">Sindh</a></nav>__SUMMARY__</header><main>__SECTIONS__<p class="reference">These are exploratory relative categories, not validated FP severity thresholds or confidence intervals. “Neither flagged” means neither score reaches the chosen cutoff. Unassessed areas remain unassessed. Source demand estimates use weighted women in need of FP; supply indices use valid-response means among confirmed FP-providing facilities. No original analysis report or regression result was changed.</p></main><div class="tooltip" role="tooltip" id="sd-tooltip"></div>
<script type="application/json" id="sd-map-data">__DATA__</script>
<script>
const DATA=JSON.parse(document.getElementById('sd-map-data').textContent),tip=document.getElementById('sd-tooltip');const mode={},selected={};const fmt=v=>v==null?'—':(v<0?'−':'+')+Math.abs(v).toFixed(3);
function legend(country){const d=DATA[country],section=document.getElementById(country),box=section.querySelector('.legend'),counts=mode[country]==='current'?d.current_counts:d.alternative_counts;box.replaceChildren();for(const [category,color] of Object.entries(d.palette)){const span=document.createElement('span');let swatch;if(color.startsWith('url(')){swatch=document.createElementNS('http://www.w3.org/2000/svg','svg');swatch.setAttribute('width','14');swatch.setAttribute('height','14');const rect=document.createElementNS('http://www.w3.org/2000/svg','rect');rect.setAttribute('width','14');rect.setAttribute('height','14');rect.setAttribute('fill',color);swatch.appendChild(rect)}else{swatch=document.createElement('i');swatch.style.background=color}swatch.setAttribute('aria-hidden','true');span.append(swatch,document.createTextNode(category+' · '+(counts[category]||0)));box.appendChild(span)}}
function update(country){const d=DATA[country],section=document.getElementById(country),r=d.areas[selected[country]],m=mode[country];section.querySelector('select').value=selected[country];section.querySelectorAll('[data-mode]').forEach(b=>b.setAttribute('aria-pressed',String(b.dataset.mode===m)));section.querySelector('[data-selected]').textContent=r.name+' · '+r[m];section.querySelector('[data-transition]').textContent='Current: '+r.current+' → Proposed: '+r.alternative;section.querySelector('[data-demand]').textContent=fmt(r.demand_u);section.querySelector('[data-supply]').textContent=fmt(r.supply_u);section.querySelector('[data-rates]').textContent=r.cpr!=null&&r.mcpr!=null?'Women in need: n = '+r.n+' · CPR '+r.cpr.toFixed(1)+'% · mCPR '+r.mcpr.toFixed(1)+'%':'No complete joint classification for this area.';section.querySelectorAll('path[data-area]').forEach(p=>{const a=d.areas[p.dataset.area];p.setAttribute('fill',d.palette[a[m]]);p.classList.toggle('selected-shape',p.dataset.area===selected[country]);p.setAttribute('aria-label',a.name+': '+a[m])});legend(country)}
function show(country,path,event){const d=DATA[country],r=d.areas[path.dataset.area],m=mode[country];tip.replaceChildren();const title=document.createElement('strong');title.textContent=r.name;tip.appendChild(title);const lines=[r[m],'Demand: '+fmt(r.demand_u)+' SD · '+r.demand_band,'Supply constraints: '+fmt(r.supply_u)+' SD · '+r.supply_band,r.cpr!=null&&r.mcpr!=null?'CPR '+r.cpr.toFixed(1)+'% · mCPR '+r.mcpr.toFixed(1)+'%':'',r.n?r.n+' women in need of FP':''];for(const text of lines){if(!text)continue;const p=document.createElement('p');p.textContent=text;tip.appendChild(p)}tip.style.display='block';const b=path.getBoundingClientRect(),x=event.clientX??b.left+b.width/2,y=event.clientY??b.top;tip.style.left=Math.max(8,Math.min(innerWidth-tip.offsetWidth-8,x+14))+'px';tip.style.top=Math.max(8,Math.min(innerHeight-tip.offsetHeight-8,y+14))+'px'}
for(const [country,d] of Object.entries(DATA)){mode[country]='alternative';selected[country]=d.default_area;const section=document.getElementById(country),select=section.querySelector('select');for(const [key,r] of Object.entries(d.areas).sort((a,b)=>a[1].name.localeCompare(b[1].name))){const option=document.createElement('option');option.value=key;option.textContent=r.name;select.appendChild(option)}select.addEventListener('change',()=>{selected[country]=select.value;tip.style.display='none';update(country)});section.querySelectorAll('[data-mode]').forEach(button=>button.addEventListener('click',()=>{mode[country]=button.dataset.mode;tip.style.display='none';update(country)}));section.querySelectorAll('path[data-area]').forEach(path=>{const choose=()=>{selected[country]=path.dataset.area;update(country)};path.addEventListener('click',choose);path.addEventListener('keydown',e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();choose()}});path.addEventListener('mouseenter',e=>show(country,path,e));path.addEventListener('mousemove',e=>show(country,path,e));path.addEventListener('mouseleave',()=>tip.style.display='none');path.addEventListener('focus',e=>show(country,path,e));path.addEventListener('blur',()=>tip.style.display='none')});update(country)}
</script></body></html>'''


def main():
    root=Path(__file__).resolve().parents[2]
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reports-dir',type=Path,default=root/'reports')
    parser.add_argument('--output',type=Path,default=root/'reports/SD_threshold_maps.html')
    parser.add_argument('--threshold',type=float,default=.5)
    args=parser.parse_args()
    if not math.isfinite(args.threshold) or args.threshold<=0:parser.error('Supply a finite positive --threshold')
    data={};sections=[];summary=[]
    for key,title,filename,unit,dn,sn,default in CONFIG:
        d,svg,changes=load_country(args.reports_dir/filename,key,title,dn,sn,args.threshold);d['default_area']=default;data[key]=d
        assert default in d['areas']
        summary.append([title,d['assessed'],d['changed'],d['assessed']-d['changed']])
        countrows=[[c,d['current_counts'].get(c,0),d['alternative_counts'].get(c,0)] for c in ORDER if c in d['palette']]
        changerows=[[r['name'],r['current'],r['alternative'],f"{r['demand_u']:+.3f}",f"{r['supply_u']:+.3f}"] for r in sorted(changes,key=lambda r:r['name'])]
        allrows=[[r['name'],fmt(r['demand_u']),r['demand_band'],fmt(r['supply_u']),r['supply_band'],r['current'],r['alternative']] for r in sorted(d['areas'].values(),key=lambda r:r['name'])]
        extra=' Five surveyed areas with unassessed supply remain unassessed; demand rescaling still uses all 82 valid demand scores.' if key=='cotedivoire' else ''
        reference=f"Reference sets: {dn} valid demand scores and {sn} valid supply scores. Final-index SDs: demand {d['reference']['demand_SD']:.6f}; supply constraints {d['reference']['supply_SD']:.6f}."+extra
        sections.append(f'''<section id="{key}"><div class="map-view" id="mapview-{key}"><div class="head"><div><h2>{title}</h2><span class="muted">{d['changed']} of {d['assessed']} assessed {unit} change category; {d['assessed']-d['changed']} remain unchanged</span></div><label>{'District' if key=='sindh' else 'Département'}<select aria-label="Select {title} area"></select></label></div><div class="controls" role="group" aria-label="{title} classification rule"><button type="button" data-mode="current" aria-pressed="false">Current: above zero</button><button type="button" data-mode="alternative" aria-pressed="true">Proposed: ≥ +{args.threshold:g} SD</button></div><p class="selected" data-selected></p><p class="muted" data-transition></p><div class="scores"><div class="score"><span>Demand · final-index SD units</span><strong data-demand></strong></div><div class="score"><span>Supply constraints · final-index SD units</span><strong data-supply></strong></div></div>{svg}<div class="legend"></div><p class="muted" data-rates></p></div><h3>Category counts</h3>{table(['Category','Current','Proposed'],countrows,(1,2))}<p class="reference">{reference}</p><details><summary>Areas whose category changes ({d['changed']})</summary>{table(['Area','Current','Proposed','Demand · SD','Supply constraints · SD'],changerows,(3,4))}</details><details><summary>All mapped areas and score bands</summary>{table(['Area','Demand · SD','Demand band','Supply · SD','Supply band','Current','Proposed'],allrows,(1,3))}</details></section>''')
    html=TEMPLATE.replace('__T__',f'{args.threshold:g}').replace('__SUMMARY__',table(['Geography','Assessed areas','Changed','Unchanged'],summary,(1,2,3))).replace('__SECTIONS__',''.join(sections)).replace('__DATA__',json.dumps(data,ensure_ascii=False,separators=(',',':')))
    assert not re.search(chr(47)+'Us'+'ers'+chr(47)+'|'+chr(47)+'ho'+'me'+chr(47)+r'|file:[/][/]|[/]private[/]var[/]',html)
    args.output.parent.mkdir(parents=True,exist_ok=True);args.output.write_text(html,encoding='utf-8')
    print(json.dumps({k:dict(assessed=d['assessed'],changed=d['changed'],reference=d['reference'],current_counts=d['current_counts'],proposed_counts=d['alternative_counts']) for k,d in data.items()},ensure_ascii=False))


def fmt(value):
    return '—' if value is None else f'{value:+.3f}'


if __name__=='__main__':main()
