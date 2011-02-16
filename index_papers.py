#!/usr/bin/python

import logging
import sys
import json
import hashlib
import metadata_extract
import crossref

logging.basicConfig(level=logging.DEBUG)

def check_doi(f):
        metad = metadata_extract.find_metadata(f)
        doi = metad.get('doi')
        if not doi:
                logging.info('Failed to find DOI in %s' % f)
                return {}
        logging.debug('DOI of %s is %s' % (f, doi))

        m = crossref.lookup_doi(doi)
        if not m:
                logging.info('Failed to get crossref data for %s' % f)
                return {}
        else:
                return m

papers = []
for f in sys.argv[1:]:
        metad = { 'filename': f }

        h = hashlib.md5()
        h.update(open(f).read())
        md5 = h.hexdigest()
        metad['md5'] = md5

        metad.update(check_doi(f))
        papers.append(metad)

json.dump(papers, open('papers.json', 'w'), indent=2)

