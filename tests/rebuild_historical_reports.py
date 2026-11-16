"""Rebuild three preserved reports with their original public source revision."""
import argparse
import hashlib
import io
import os
from pathlib import Path
import subprocess
import tarfile
import urllib.request

REVISION = 'd2452b7f364f449ae6824db7a312a4217de8858b'
ARCHIVE_SHA256 = 'ee14e2267c4b40a557e94df1b26b3a43b4fe24a311bab74e434d5c917598aa93'
URL = 'https://codeload.github.com/graciasierra6/Surgo-manitoba/tar.gz/' + REVISION

ap = argparse.ArgumentParser()
for name in ['input-zip', 'join-dir', 'cache-dir', 'work-dir', 'output-dir']:
    ap.add_argument('--'+name, required=True)
ap.add_argument('--python', default='python3')
ap.add_argument('--countries', default='lagos,senegal,cotedivoire')
args = ap.parse_args()
work = Path(args.work_dir).resolve()
work.mkdir(parents=True, exist_ok=True)
archive = work/'historical_source.tar.gz'
if not archive.exists():
    with urllib.request.urlopen(URL, timeout=120) as response:
        archive.write_bytes(response.read())
assert hashlib.sha256(archive.read_bytes()).hexdigest() == ARCHIVE_SHA256, 'Historical source checksum mismatch'
with tarfile.open(archive) as package:
    for member in package.getmembers():
        target = (work/member.name).resolve()
        assert work in target.parents and not member.issym() and not member.islnk(), 'Unsafe source archive'
    package.extractall(work)
source = work/('Surgo-manitoba-'+REVISION)
command = ['Rscript', str(source/'R/run_reports.R'), '--input-zip', str(Path(args.input_zip).resolve()),
           '--join-dir', str(Path(args.join_dir).resolve()), '--cache-dir', str(Path(args.cache_dir).resolve()),
           '--work-dir', str(work/'calculation'), '--output-dir', str(Path(args.output_dir).resolve()),
           '--countries', args.countries, '--python', args.python]
subprocess.run(command, cwd=source, check=True, env=os.environ.copy())
