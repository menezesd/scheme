"""Checks that the differential runner cannot hide semantic differences."""

import contextlib
import io
import subprocess
import unittest
from unittest.mock import patch

import run_mit_differential as runner


class DifferentialComparisonTests(unittest.TestCase):
    def agrees(self, left, right):
        return runner.equivalent(runner.normalize(left), runner.normalize(right))

    def test_literal_case_is_significant(self):
        for left, right in [('"A"', '"a"'), (r'#\A', r'#\a'),
                            ('|A|', '|a|'), ('fooA', 'fooa')]:
            with self.subTest(left=left, right=right):
                self.assertFalse(self.agrees(left, right))

    def test_numeric_looking_literals_are_exact(self):
        for left, right in [('"1."', '"1.0"'), ('foo1.', 'foo1.0'),
                            ('"1.0"', '"1.0000000000001"'),
                            ('|1.0|', '|1.0000000000001|')]:
            with self.subTest(left=left, right=right):
                self.assertFalse(self.agrees(left, right))

    def test_number_spelling_and_roundoff_are_tolerated(self):
        self.assertTrue(self.agrees('(.5 1. 1.E+07 +2i)', '(0.5 1.0 1.0e7 0+2i)'))
        self.assertTrue(self.agrees('(1.0 "A")', '(1.0000000000001 "A")'))
        self.assertFalse(self.agrees('(1.0 "A")', '(1.0000000000001 "a")'))
        self.assertFalse(self.agrees('1.0e400', '1.0e308'))

    def test_escaped_literals_and_quote_characters_remain_data(self):
        value = r'(#\" "a\"1." |x\|1.| #\) 1.)'
        self.assertEqual(runner.normalize(value),
                         r'(#\" "a\"1." |x\|1.| #\) 1.0)')

    def test_signed_zero_is_significant(self):
        self.assertFalse(self.agrees('0.0', '-0.0'))
        self.assertFalse(self.agrees('1.0+0.0i', '1.-0.i'))
        self.assertTrue(self.agrees('1.0-0.0i', '1.-0.i'))

    def test_failed_process_cannot_pass_after_printing_expected_output(self):
        child = subprocess.CompletedProcess([], 1, stdout='7\n', stderr='failed')
        output = io.StringIO()
        with patch.object(runner, 'PROBES', ['7']), \
             patch.object(runner.shutil, 'which', return_value='mit-scheme'), \
             patch.object(runner.os.path, 'exists', return_value=True), \
             patch.object(runner.subprocess, 'run', return_value=child), \
             contextlib.redirect_stdout(output):
            self.assertEqual(runner.main(), 1)
        self.assertIn('FAIL: vm: exited with status 1', output.getvalue())

    def test_timeout_is_a_failure(self):
        with patch.object(runner.subprocess, 'run',
                          side_effect=subprocess.TimeoutExpired('scheme', 1)):
            with self.assertRaisesRegex(RuntimeError, 'timed out'):
                runner.run_scheme(lambda path: ['scheme', path], '7', timeout=1)


if __name__ == '__main__':
    unittest.main()
