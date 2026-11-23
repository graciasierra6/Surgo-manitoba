"""Populate the authored data-check layout with freshly calculated values."""
import argparse
import json
import re
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('--values', required=True)
parser.add_argument('--output', required=True)
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
template = (root/'support/assets/data_checks_template.html').read_text()
values = json.loads(Path(args.values).read_text())
tokens = set(re.findall(r'\{\{([0-9_]+)\}\}', template))
assert tokens == set(values), ('Missing or unused calculated cells', tokens ^ set(values))
for key, value in values.items():
    template = template.replace('{{'+key+'}}', value)
assert '{{' not in template
output = Path(args.output)
output.parent.mkdir(parents=True, exist_ok=True)
output.write_text(template)
