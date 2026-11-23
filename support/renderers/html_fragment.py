"""Extract a complete report's inline style and body for R Markdown inclusion."""
import argparse
import re
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('--input', required=True)
parser.add_argument('--output', required=True)
args = parser.parse_args()
source = Path(args.input).read_text(encoding='utf-8')
head = re.search(r'<head[^>]*>([\s\S]*?)</head>', source, re.I).group(1)
body = re.search(r'<body[^>]*>([\s\S]*?)</body>', source, re.I).group(1)
styles = '\n'.join(re.findall(r'<style[^>]*>[\s\S]*?</style>', head, re.I))
assert styles and '<script>' in body
Path(args.output).write_text(styles + '\n' + body + '\n', encoding='utf-8')
