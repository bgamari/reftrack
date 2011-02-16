#!/usr/bin/python

import os
import logging
import sys
import json
import hashlib
import metadata_extract
import crossref
import arxiv

logging.basicConfig(level=logging.DEBUG)

def get_metadata(f):
        h = hashlib.md5()
        h.update(open(f).read())
        md5 = h.hexdigest()
        metad = { 'filename': f, 'md5': md5 }

        metad.update(metadata_extract.find_metadata(f))
        doi = metad.get('doi')
        arxiv_id = metad.get('arxiv_id')

        if doi:
                logging.debug('DOI of %s is %s' % (f, doi))
                m = crossref.lookup_doi(doi)
                if m:
                        metad.update(m)
                else:
                        logging.info('Failed to get crossref metadata for %s' % f)

        elif arxiv_id:
                logging.debug('arXiv ID of %s is %s' % (f, arxiv_id))
                m = arxiv.get_metadata(arxiv_id)
                if m:
                        metad.update(m)
                else:
                        logging.info('Failed to get arXiv metadata for %s' % f)

        else:
                logging.info('Could not identify %s' % f)

        return metad

def merge_papers(p1, p2):
        d1 = dict([ (p['md5'], p) for p in p1 ])
        d2 = dict([ (p['md5'], p) for p in p2 ])

        res = []
        for k in d1.keys()+d2.keys():
                r = {}
                r.update(d1.get(k, {}))
                r.update(d2.get(k, {}))
                res.append(r)

        return res

papers = []
for f in sys.argv[1:]:
        try:
                papers.append(get_metadata(f))
        except Exception as e:
                print e
                pass

if os.path.isfile('papers.json'):
        old_papers = json.load(open('papers.json'))
        papers = merge_papers(old_papers, papers)

json.dump(papers, open('papers.json', 'w'), indent=2)

