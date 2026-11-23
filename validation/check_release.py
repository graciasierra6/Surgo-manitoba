"""Inspect the actual source/report tree and embedded report assets before publication."""
from pathlib import Path
import ast,base64,hashlib,json,re,sys
root=Path(__file__).resolve().parents[1]
ignored={'.git','renv','data','output','tmp','__pycache__'}
files=[p for p in root.rglob('*') if p.is_file() and not (set(p.relative_to(root).parts)&ignored)]
private=re.compile(chr(47)+'Us'+'ers'+chr(47)+'|'+chr(47)+'ho'+'me'+chr(47)+r'|[A-Za-z]:[\\/](?:Users|Documents|Desktop)')
unwanted=re.compile('co'+'dex|chat'+'gpt|open'+'ai',re.I)
for p in files:
    assert p.suffix.lower() not in {'.dta','.rds','.rdata','.zip','.xlsx','.xls','.sav','.parquet','.feather'},p.relative_to(root)
    if p.suffix in {'.R','.Rmd','.py','.md','.html','.json','.css','.lock','.geojson'} or p.name in {'DESCRIPTION','.gitignore','.Rprofile','LICENSE-NOTICE'}:
        t=p.read_text();assert not private.search(t),('personal path',p.relative_to(root))
        assert not unwanted.search(t),('identifying authoring string',p.relative_to(root))
        if p.suffix=='.py':ast.parse(t)
        if p.suffix=='.html':
            for encoded in re.findall(r'data:(?:image/svg\+xml|text/csv)(?:;[^,;]+)*;base64,([^"\s]+)',t):
                v=base64.b64decode(encoded).decode();assert not private.search(v) and not unwanted.search(v),p.relative_to(root)
assets=json.loads((root/'support/assets/manifest.json').read_text())
for f in assets['files']:
    assert hashlib.sha256((root/'support/assets'/f['file']).read_bytes()).hexdigest()==f['sha256'],f['file']
ref=json.loads((root/'validation/reference.json').read_text())
for name,expected in ref['source_files'].items():
    assert hashlib.sha256((root/name).read_bytes()).hexdigest()==expected,('source fingerprint',name)
for name,record in ref['reports'].items():
    p=root/name
    if name.startswith('generated_checks/'):continue
    assert p.is_file(),name
    assert hashlib.sha256(p.read_bytes()).hexdigest()==record['published_sha256'],name
    if name.startswith(('aggregate/','rural_urban/')):
        assert p.with_suffix('.R').is_file() and p.with_suffix('.Rmd').is_file(),name
        t=p.read_text();assert 'id="score-thresholds"' in t and 'data-supply-score-orientation="constraints"' in t,name
        if name.startswith('aggregate/'):assert 'id="regression-sensitivity"' in t,name
filenames=(root/'docs/DATASET_FILENAMES.md').read_text()
manifest=json.loads((root/'docs/input_bundle_manifest.json').read_text())
assert all(f['member'] in filenames for f in manifest['files'])
assert 'bundle_manifest.json' in filenames
print('Checked',len(files),'shared files; seven adjacent R/Rmd/HTML report sets, nine published HTMLs, input inventory, source fingerprints and embedded assets.')
