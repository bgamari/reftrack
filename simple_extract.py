#!/usr/bin/python

import logging
import subprocess
import re

doi_re = re.compile(r'(?:doi|DOI)?[\s\.\:]{0,3}(10\.\d{4}/[\d\w\-\.\+]+)')

def find_doi(file):
        p = subprocess.Popen(['pdftotext', file, '-'], stdout=subprocess.PIPE)
        txt = p.stdout.read()
        m = doi_re.search(txt)
        if m:
                doi = m.group(1)
                return doi

        hi = re.search(r'10\..+', txt)
        if hi and not m:
                logging.info('Found possible non-matching doi: %s' % hi.group(0))
        return None

if __name__ == '__main__':
        import sys
        for f in sys.argv[1:]:
                print f, find_doi(f)

