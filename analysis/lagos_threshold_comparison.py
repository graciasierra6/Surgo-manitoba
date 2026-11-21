"""Compare current Lagos zero flags with a positive final-index SD threshold.

The published report supplies the unchanged indices, categories and boundaries.
Only final-index rescaling and the exploratory flag threshold are applied here.
Run: python3 analysis/lagos_threshold_comparison.py --threshold 0.5
"""
import argparse
from collections import Counter
from html import escape
import json
import math
from pathlib import Path
import re
import statistics
import xml.etree.ElementTree as ET

from new_maps import CATEGORIES, load_map


def band(value):
    if value is None:
        return 'Unassessed'
    return ('Relatively fewer challenges' if value < -.5 else 'Around average' if value < .5
            else 'Moderately elevated' if value < 1 else 'Highly elevated')


def classify(demand, supply):
    return 'Both' if demand and supply else 'Demand only' if demand else 'Supply only' if supply else 'Neither flagged'


TEMPLATE = r'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Lagos: threshold comparison</title>
<style>
:root{--ink:#17374a;--muted:#58717f;--line:#dbe5ea}*{box-sizing:border-box}body{margin:0;background:#f3f6f8;color:var(--ink);font:15px/1.5 Arial,Helvetica,sans-serif}main{max-width:1500px;margin:auto;padding:28px}h1{font-size:28px;margin:0 0 8px}h2{font-size:21px;margin:0 0 5px}h3{font-size:18px;margin:0 0 8px}p{margin:7px 0}.intro{max-width:1050px}.muted{font-size:13px;color:var(--muted)}.selection{display:flex;gap:16px;flex-wrap:wrap;align-items:center;margin:20px 0 14px}select{font:inherit;padding:8px 12px;border:1px solid #c4d4dd;border-radius:5px;background:white;color:var(--ink)}select:focus-visible{outline:3px solid #8cc0c7;outline-offset:2px}.grid{display:grid;grid-template-columns:1fr 1fr;gap:20px}.panel,.table-section{background:white;border:1px solid var(--line);border-radius:8px;padding:20px}.panel .category{font-size:18px;font-weight:600;margin:12px 0 7px}.scores{display:flex;gap:18px;flex-wrap:wrap;font-size:13px}.scores strong{font-size:17px;display:block;font-variant-numeric:tabular-nums}.geo-map{display:block;width:100%;height:auto;margin:14px 0 8px}.geo-map [data-area]{cursor:pointer;stroke:white;stroke-width:1.1;vector-effect:non-scaling-stroke}.geo-map [data-area].selected{stroke:var(--ink);stroke-width:2.2}.geo-map [data-area]:focus{stroke:var(--ink);stroke-width:2.2;outline:none}.geo-map .major-label text{fill:var(--ink);font-family:Arial,Helvetica,sans-serif;paint-order:stroke;stroke:white;stroke-width:4;stroke-linejoin:round;font-weight:500;pointer-events:none}.legend{display:flex;gap:9px 14px;flex-wrap:wrap;font-size:12px}.legend span{display:inline-flex;align-items:center;gap:5px}.legend i{display:inline-block;width:12px;height:12px;border:1px solid #d5dfe5}.table-section{margin-top:22px}table{width:100%;border-collapse:collapse;font-size:14px}th,td{text-align:left;padding:10px 11px;border-bottom:1px solid #e4ebef}th{font-weight:600;color:var(--ink)}td.number{text-align:right;font-variant-numeric:tabular-nums}.table-wrap{overflow-x:auto}.status{font-size:14px;font-weight:600;margin:10px 0}.tooltip{display:none;position:fixed;z-index:50;pointer-events:none;background:var(--ink);color:white;padding:12px 15px;max-width:300px;border-radius:5px;font-size:13px}.tooltip p{margin:5px 0}.tooltip strong{font-size:15px}.note{margin-top:16px;color:var(--muted);font-size:13px}.changed td:first-child{font-weight:600}@media(max-width:1100px){.grid{grid-template-columns:1fr}}@media(max-width:600px){main{padding:16px 10px}.panel,.table-section{padding:15px}h1{font-size:24px}.scores{gap:14px}th,td{padding:8px}table{font-size:13px}}@media print{body{background:white}.grid{grid-template-columns:1fr}.panel{break-inside:avoid}.tooltip{display:none}}
</style></head><body><main>
<h1>Lagos: current classification versus +__THRESHOLD__ SD</h1>
<p class="intro">Same six demand indicators and six supply indicators, same weights and FP-provider restriction, same composite indices. Both scores are expressed in standard deviations of their respective final indices; higher means more challenges.</p>
<p class="status">__CHANGED__ of 17 assessed LGAs change category; __UNCHANGED__ remain unchanged.</p>
<div class="selection"><label for="selected-area">LGA</label><select id="selected-area"></select><span id="selected-transition"></span></div>
<div class="grid" id="comparison-maps">__PANELS__</div>
<section class="table-section"><h2>How the category counts change</h2>__COUNTS__</section>
<section class="table-section"><h2>LGAs whose category changes</h2><p class="muted">Scores below are in final-index SD units. The alternative flags each side at +__THRESHOLD__ SD or higher.</p>__CHANGES__</section>
<section class="table-section"><h2>Thresholds and interpretation</h2><p>Final demand-index SD: <b>__D_SD__</b>. Final supply-constraints-index SD: <b>__S_SD__</b>, calculated equally across the same 17 assessed LGAs. Their means are approximately zero.</p><p>The proposed cutoff corresponds to an original demand score of <b>__D_CUT__</b> and an original supply-constraints score of <b>__S_CUT__</b>.</p><p>Descriptive bands: below −0.5 SD, relatively fewer challenges; −0.5 to below +0.5, around average; +0.5 to below +1, moderately elevated; +1 or higher, highly elevated. These are exploratory relative bands, not validated FP severity thresholds or confidence intervals.</p><p>“Neither flagged” means neither score reaches the selected cutoff. Three LGAs with supply data only remain unassessed under both rules. Original main reports and model results are unchanged.</p><details><summary>All 17 assessed LGAs</summary>__ALL_AREAS__</details></section>
<p class="note">Source: stored scores and display boundaries from the current Lagos aggregate report. Demand percentages underlying the scores use survey weights among women in need of FP; supply measures use valid responses among confirmed FP providers.</p>
</main><div class="tooltip" id="threshold-tooltip" role="tooltip"></div>
<script id="threshold-data" type="application/json">__DATA__</script>
<script>
const DATA=JSON.parse(document.getElementById('threshold-data').textContent);let selected='IBEJULEKKI';const select=document.getElementById('selected-area'),tip=document.getElementById('threshold-tooltip');
const fmt=v=>v==null?'—':(v<0?'−':'+')+Math.abs(v).toFixed(3);
for(const [key,row] of Object.entries(DATA.areas).sort((a,b)=>a[1].name.localeCompare(b[1].name))){const option=document.createElement('option');option.value=key;option.textContent=row.name;select.appendChild(option)}
function update(){select.value=selected;const row=DATA.areas[selected];document.getElementById('selected-transition').textContent=row.name+': '+row.current+' → '+row.alternative;for(const mode of ['current','alternative']){const panel=document.getElementById(mode);panel.querySelector('[data-category]').textContent=row.name+' · '+row[mode];panel.querySelector('[data-demand]').textContent=fmt(row.demand_u);panel.querySelector('[data-supply]').textContent=fmt(row.supply_u);panel.querySelectorAll('[data-area]').forEach(path=>{const r=DATA.areas[path.dataset.area];path.setAttribute('fill',DATA.palette[r[mode]]);path.classList.toggle('selected',path.dataset.area===selected);path.setAttribute('aria-label',r.name+': '+r[mode])})}}
function tooltip(mode,path,event){const r=DATA.areas[path.dataset.area];tip.replaceChildren();const title=document.createElement('strong');title.textContent=r.name;tip.appendChild(title);const lines=[r[mode],r.assessed?'Demand: '+fmt(r.demand_u)+' SD · '+r.demand_band:'No complete demand/supply classification',r.assessed?'Supply constraints: '+fmt(r.supply_u)+' SD · '+r.supply_band:'Supply data only',r.assessed?'CPR '+r.cpr.toFixed(1)+'% · mCPR '+r.mcpr.toFixed(1)+'%':''];for(const text of lines){if(!text)continue;const p=document.createElement('p');p.textContent=text;tip.appendChild(p)}tip.style.display='block';const box=path.getBoundingClientRect(),x=event.clientX??box.left+box.width/2,y=event.clientY??box.top;tip.style.left=Math.max(8,Math.min(innerWidth-tip.offsetWidth-8,x+14))+'px';tip.style.top=Math.max(8,Math.min(innerHeight-tip.offsetHeight-8,y+14))+'px'}
for(const mode of ['current','alternative']){const panel=document.getElementById(mode);panel.querySelectorAll('[data-area]').forEach(path=>{path.addEventListener('mouseenter',e=>tooltip(mode,path,e));path.addEventListener('mousemove',e=>tooltip(mode,path,e));path.addEventListener('mouseleave',()=>tip.style.display='none');path.addEventListener('focus',e=>tooltip(mode,path,e));path.addEventListener('blur',()=>tip.style.display='none');const choose=()=>{selected=path.dataset.area;update()};path.addEventListener('click',choose);path.addEventListener('keydown',e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();choose()}})})}
function labelSizes(){document.querySelectorAll('.geo-map').forEach(svg=>{const width=svg.clientWidth;svg.querySelectorAll('.major-label text').forEach(text=>{text.style.fontSize=(14*1400/width)+'px';text.parentElement.style.display=width<460&&!['BADAGRY','EPE'].includes(text.parentElement.dataset.key)?'none':''})})}
select.addEventListener('change',()=>{selected=select.value;tip.style.display='none';update()});window.addEventListener('resize',labelSizes);update();labelSizes();
</script></body></html>'''


def table(headers, rows, numeric=()):
    return '<div class="table-wrap"><table><thead><tr>'+''.join('<th>'+escape(x)+'</th>' for x in headers)+'</tr></thead><tbody>'+''.join('<tr>'+''.join('<td'+(' class="number"' if j in numeric else '')+'>'+escape(str(v))+'</td>' for j,v in enumerate(row))+'</tr>' for row in rows)+'</tbody></table></div>'


def main():
    root=Path(__file__).resolve().parents[1]
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reports-dir',type=Path,default=root/'reports')
    parser.add_argument('--output',type=Path,default=root/'reports/Lagos_threshold_comparison.html')
    parser.add_argument('--threshold',type=float,default=.5)
    args=parser.parse_args()
    if not math.isfinite(args.threshold) or args.threshold<=0:
        parser.error('--threshold must be a finite positive SD distance')
    info,svg_text=load_map(args.reports_dir/'Lagos_analysis_aggregate.html','lagos',17)
    rows=[r for r in info['areas'].values() if r['assessed']]
    dm=statistics.mean(r['demand'] for r in rows);sm=statistics.mean(r['constraints'] for r in rows)
    ds=statistics.stdev(r['demand'] for r in rows);ss=statistics.stdev(r['constraints'] for r in rows)
    for r in info['areas'].values():
        r['current']=r['category'];r['alternative']=r['category'];r['demand_u']=r['supply_u']=None
        if r['assessed']:
            r['demand_u']=(r['demand']-dm)/ds;r['supply_u']=(r['constraints']-sm)/ss
            current_d=r['demand']>1e-10;current_s=r['constraints']>1e-10
            assert classify(current_d,current_s)==r['current']
            new_d=r['demand_u']>=args.threshold;new_s=r['supply_u']>=args.threshold
            assert not new_d or current_d
            assert not new_s or current_s
            r['alternative']=classify(new_d,new_s)
        r['demand_band']=band(r['demand_u']);r['supply_band']=band(r['supply_u'])
    changes=[r for r in rows if r['current']!=r['alternative']]
    old_counts=Counter(r['current'] for r in info['areas'].values());new_counts=Counter(r['alternative'] for r in info['areas'].values())
    panels=[]
    for mode,title,rule,counts in [('current','Current: above zero','Flag demand > 0; flag supply constraints > 0',old_counts),('alternative',f'Proposed: +{args.threshold:g} SD or higher',f'Flag demand ≥ {args.threshold:g} SD; flag supply constraints ≥ {args.threshold:g} SD',new_counts)]:
        svg=ET.fromstring(svg_text);svg.set('id','map-'+mode);svg.set('aria-label',title+' Lagos classification')
        for parent in svg.iter():
            for child in list(parent):
                if child.tag.rsplit('}',1)[-1] in ['polyline','rect'] or child.get('class')=='lga-name':
                    if child.get('class')=='lga-name':
                        if child.get('data-key') in ['BADAGRY','EPE','IKORODU','IBEJULEKKI']:
                            child.set('class','major-label')
                            for x in list(child):
                                if x.tag.rsplit('}',1)[-1]!='text':child.remove(x)
                        else:parent.remove(child)
            if parent.get('data-area'):
                parent.set('fill',info['palette'][info['areas'][parent.get('data-area')][mode]])
        # Remove the north arrow and scale annotations, leaving the unchanged geographic paths.
        for child in list(svg):
            if child.tag.rsplit('}',1)[-1] in ['path','text'] and not child.get('data-area'):
                svg.remove(child)
        geometry=ET.tostring(svg,encoding='unicode')
        legend=''.join('<span><i style="background:'+info['palette'][c]+'"></i>'+escape(c)+' · '+str(counts.get(c,0))+'</span>' for c in CATEGORIES)
        panels.append(f'<section class="panel" id="{mode}"><h2>{title}</h2><p class="muted">{rule}</p><p class="category" data-category></p><div class="scores"><div>Demand · SD units<strong data-demand></strong></div><div>Supply constraints · SD units<strong data-supply></strong></div></div>{geometry}<div class="legend">{legend}</div></section>')
    changed_rows=[[r['name'],r['current'],r['alternative'],f"{r['demand_u']:+.3f}",f"{r['supply_u']:+.3f}"] for r in sorted(changes,key=lambda r:r['name'])]
    all_rows=[[r['name'],f"{r['demand_u']:+.3f}",r['demand_band'],f"{r['supply_u']:+.3f}",r['supply_band'],r['current'],r['alternative']] for r in sorted(rows,key=lambda r:r['name'])]
    counts_rows=[[c,old_counts.get(c,0),new_counts.get(c,0)] for c in CATEGORIES]
    payload={'areas':info['areas'],'palette':info['palette'],'threshold_SD':args.threshold,'means':{'demand':dm,'supply_constraints':sm},'SDs':{'demand':ds,'supply_constraints':ss},'changed':len(changes),'current_counts':dict(old_counts),'alternative_counts':dict(new_counts)}
    replacements={'__THRESHOLD__':f'{args.threshold:g}','__CHANGED__':str(len(changes)),'__UNCHANGED__':str(17-len(changes)),
        '__PANELS__':''.join(panels),'__COUNTS__':table(['Category','Current','Proposed'],counts_rows,(1,2)),
        '__CHANGES__':table(['LGA','Current','Proposed','Demand · SD units','Supply · SD units'],changed_rows,(3,4)),
        '__ALL_AREAS__':table(['LGA','Demand · SD','Demand band','Supply · SD','Supply band','Current','Proposed'],all_rows,(1,3)),
        '__D_SD__':f'{ds:.6f}','__S_SD__':f'{ss:.6f}','__D_CUT__':f'{dm+args.threshold*ds:.6f}',
        '__S_CUT__':f'{sm+args.threshold*ss:.6f}','__DATA__':json.dumps(payload,ensure_ascii=False,separators=(',',':'))}
    html=TEMPLATE
    for key,value in replacements.items():html=html.replace(key,value)
    assert not re.search(chr(47)+'Us'+'ers'+chr(47)+'|'+chr(47)+'ho'+'me'+chr(47)+r'|file:[/][/]|[/]private[/]var[/]',html)
    args.output.parent.mkdir(parents=True,exist_ok=True);args.output.write_text(html,encoding='utf-8')
    print(json.dumps({'assessed_LGAs':17,'changed':len(changes),'current_counts':dict(old_counts),'proposed_counts':dict(new_counts),'original_unit_cutoffs':{'demand':dm+args.threshold*ds,'supply_constraints':sm+args.threshold*ss}}))


if __name__=='__main__':main()
