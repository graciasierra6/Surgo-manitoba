"""The report comparison must reject substantive changes while tolerating roundoff."""
import unittest
import hashlib
from pathlib import Path
import tempfile
from verify_rebuilt_reports import numeric_text_equal, tables_equal, compare_report

class Equivalence(unittest.TestCase):
    def test_roundoff(self):
        self.assertTrue(numeric_text_equal('x = -0.123456789;', 'x = -0.12345678900001;'))
    def test_effect_change(self):
        self.assertFalse(numeric_text_equal('x = -0.123;', 'x = -0.124;'))
    def test_count_change(self):
        self.assertFalse(numeric_text_equal('n = 100000000;', 'n = 100000001;'))
    def test_label_change(self):
        self.assertFalse(numeric_text_equal('demand: 0.1', 'supply: 0.1'))
    def test_table_identifiers(self):
        self.assertFalse(tables_equal([[['geo','estimate'],['01','0.1']]],
                                      [[['geo','estimate'],['1','0.1']]]))
    def test_table_roundoff(self):
        self.assertTrue(tables_equal([[['geo','estimate'],['A','0.123456789']]],
                                     [[['geo','estimate'],['A','0.12345678900001']]]))
    def test_generated_only_exact_hash(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); rebuilt = root/'rebuilt.html'
            rebuilt.write_bytes(b'<p>Validated supplement</p>')
            expected = hashlib.sha256(rebuilt.read_bytes()).hexdigest()
            self.assertTrue(compare_report(root/'absent.html', rebuilt, expected)['passed'])
            self.assertFalse(compare_report(root/'absent.html', rebuilt, '0'*64)['passed'])
    def test_missing_published_reference_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); rebuilt = root/'rebuilt.html'
            rebuilt.write_bytes(b'<p>Output</p>')
            self.assertFalse(compare_report(root/'absent.html', rebuilt)['passed'])
    def test_missing_generated_output_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.assertFalse(compare_report(root/'absent.html', root/'output.html', '0'*64)['passed'])

if __name__ == '__main__': unittest.main()
