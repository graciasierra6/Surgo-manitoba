"""Compatibility entry point for the current Sindh district-report checks."""
from pathlib import Path
import subprocess
import sys
root=Path(__file__).resolve().parents[1]
subprocess.run([sys.executable,str(root/'administrative_area/analysis/sindh/verify_html.py'),*sys.argv[1:]],check=True)
