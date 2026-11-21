"""The report comparison must reject substantive changes while tolerating roundoff."""
import unittest
import hashlib
from pathlib import Path
import tempfile
from verify_rebuilt_reports import numeric_text_equal, tables_equal, compare_report, content_signature, compare_validated_update

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
    def test_embedded_map_values_checked(self):
        a = '<iframe srcdoc="&lt;script&gt;const threshold=0.5;&lt;/script&gt;"></iframe>'
        b = a.replace('0.5', '1.5')
        self.assertNotEqual(content_signature(a), content_signature(b))
    def test_source_digest_is_separate_from_analytical_values(self):
        a = '<script>{"source_sha256":"'+('a'*64)+'","estimate":0.2}</script>'
        b = a.replace('a'*64, 'b'*64)
        self.assertEqual(content_signature(a), content_signature(b))
        self.assertNotEqual(content_signature(a), content_signature(b.replace('0.2','0.3')))
    def test_code_edition_preserves_published_reference(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); reference = root/'published.html'; rebuilt = root/'new.html'
            reference.write_text('<p>Previous edition</p>'); rebuilt.write_text('<p>New direction</p>')
            record = dict(published_sha256=hashlib.sha256(reference.read_bytes()).hexdigest(),
                          content_signature=content_signature(rebuilt.read_text()))
            self.assertTrue(compare_validated_update(reference, rebuilt, record)['passed'])
            rebuilt.write_text('<p>Different result</p>')
            self.assertFalse(compare_validated_update(reference, rebuilt, record)['passed'])
            reference.write_text('<p>Changed published reference</p>')
            self.assertFalse(compare_validated_update(reference, rebuilt, record)['passed'])

if __name__ == '__main__': unittest.main()
