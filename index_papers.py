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

total = 0
unidentified = 0

def get_metadata(f):
        global total, unidentified
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
                unidentified += 1
                logging.info('Could not identify %s' % f)

        total += 1
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

def process_files(files):
        for f in files:
                try:
                        p = get_metadata(f)
                        yield p
                except Exception as e:
                        logging.warn('Error processing %s: %s' % (f, e))

        logging.info('Processed %d files, %d unidentified (identified %3.1f%%)' %
                     (total, unidentified, 100.*(total-unidentified)/total))

def mongo_update(refs):
        import mongoengine, schema
        mongoengine.connect('refs')
        for p in refs:
                d = schema.Ref.objects(md5=p['md5']).first()
                print p
                if p.get('type') == 'journal_article':
                        del p['type']
                        p['authors'] = [ '%s %s' % (given, sur) for given,sur in p['authors'] ]
                        if not d:
                                d = schema.JournalArticle(**p)
                        else:
                                for k in p:
                                        if k not in d: d[k] = p[k]
                        d.save()

def json_dump(files):
        papers = list(process_files(files))
        if os.path.isfile('papers.json'):
                old_papers = json.load(open('papers.json'))
                papers = merge_papers(old_papers, papers)

        json.dump(papers, open('papers.json', 'w'), indent=2)

if __name__ == '__main__':
        #json_dump(sys.argv[1:])
        mongo_update(process_files(sys.argv[1:]))

