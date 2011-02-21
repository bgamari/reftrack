#!/usr/bin/python

import urllib2
import xml.etree.ElementTree

arxiv_oai_url = 'http://export.arxiv.org/oai2'

def oai_request(url, query):
        while True:
                r = urllib2.urlopen('%s?%s' % (url, query))
                if r.code == 503:
                        from time import sleep
                        wait = float(r.info()['Retry-After'])
                        sleep(wait)
                if r.code == 200:
                        return r.read()
                else:
                        raise RuntimeError('Failed urllib2 request')

def get_metadata(arxiv_id):
        query = 'verb=GetRecord&identifier=oai:arXiv.org:%s&metadataPrefix=arXiv' % arxiv_id
        d = oai_request(arxiv_oai_url, query)

        # Hack around annoying root xmlns=... attributes
        d = d.replace('xmlns="http://www.openarchives.org/OAI/2.0/"', '') 
        d = d.replace('xmlns="http://arxiv.org/OAI/arXiv/"', '') 
        et = xml.etree.ElementTree.fromstring(d)

        md = { 'arxiv_id': arxiv_id }
        m = et.find('GetRecord/record/metadata/arXiv')
        md['authors'] = [ (a.findtext('forenames'), a.findtext('keyname'))
                         for a in m.findall('authors/author') ]
        md['title'] = m.findtext('title')
        md['abstract'] = m.findtext('abstract').strip()
        return md

if __name__ == '__main__':
        arxiv_id = '0804.2273'
        metad = get_metadata(arxiv_id)
        print metad

