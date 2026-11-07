"""Check upload contents, report structure and portable source references."""
import ast
import base64
import hashlib
import json
import re
from pathlib import Path

root = Path(__file__).resolve().parents[1]
exclude = {'.git', 'output', 'output_urban_rural', 'data', 'tmp', '__pycache__', 'library', 'staging'}
files = [p for p in root.rglob('*') if p.is_file() and not (set(p.relative_to(root).parts) & exclude)
         and not (p.relative_to(root).parts[0] == 'reports' and any(x.endswith('_files') for x in p.parts))
         and p.name not in {'.DS_Store', 'Rplots.pdf'} and p.suffix not in {'.pyc', '.log'}]
private = re.compile(chr(47)+'Us'+'ers'+chr(47)+'|'+chr(47)+'ho'+'me'+chr(47)+
                     r'|[A-Za-z]:[\\/](?:Users|Documents|Desktop)')
assistant = re.compile('co'+'dex|chat'+'gpt|open'+'ai', re.I)
text_suffixes = {'.R', '.Rmd', '.py', '.md', '.html', '.css', '.json', '.csv', '.geojson', '.yml', '.yaml', '.lock'}
for p in files:
    assert p.suffix.lower() not in {'.dta', '.rds', '.rdata', '.zip'}, p
    assert p.stat().st_size < 95_000_000, p
    if p.suffix in text_suffixes or p.name in {'DESCRIPTION','.gitignore','.Rprofile','LICENSE-NOTICE'}:
        text = p.read_text(encoding='utf-8')
        assert not private.search(text), ('private path', p)
        assert not assistant.search(text), ('unwanted provenance string', p)
        if p.suffix == '.py': ast.parse(text, filename=str(p))
        if p.suffix == '.html':
            for encoded in re.findall(r'data:(?:image/svg\+xml|text/csv)(?:;[^,;]+)*;base64,([^"\s]+)', text):
                decoded = base64.b64decode(encoded).decode('utf-8')
                assert not private.search(decoded) and not assistant.search(decoded), ('embedded content', p)
manifest = json.loads((root/'analysis/assets/manifest.json').read_text())
for q in manifest['files']:
    p = root/'analysis/assets'/q['file']
    assert hashlib.sha256(p.read_bytes()).hexdigest() == q['sha256'], p
reference = json.loads((root/'tests/report_reference_values.json').read_text())
for name, expected in reference['_existing_join_files'].items():
    assert hashlib.sha256((root/name).read_bytes()).hexdigest() == expected, ('existing join changed', name)
report_names = ['Lagos_analysis','Sindh_analysis','Senegal_analysis','Cote_dIvoire_analysis']
countries = ['lagos','sindh','senegal','cotedivoire']
for country, name in zip(countries, report_names):
    assert (root/'analysis'/country/(name+'.Rmd')).is_file()
    text = (root/'reports'/(name+'.html')).read_text()
    assert text.lower().startswith('<!doctype html>') and '<script>' in text
    assert not re.search(r'<(?:script|img)[^>]+(?:src)=["\']https?://', text)
    assert len(re.findall(r'data:image/svg\+xml;base64,', text)) == 10
for module in ['administrative_area','urban_rural']:
    assert (root/module/'R'/('run_join.R' if module == 'administrative_area' else 'run_join_urban_rural.R')).is_file()
    assert (root/module/'R/00_config.R').is_file()
    assert (root/module/'README.md').is_file()
print(f'Checked {len(files)} upload files: four offline reports/Rmds, separate joins, asset checksums, portable paths and source syntax.')
