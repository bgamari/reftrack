#!/usr/bin/python2.7

import urllib2
from xml.etree import ElementTree

arxiv_oai_url = 'http://export.arxiv.org/oai2'

prefixes = {'oai': 'http://www.openarchives.org/OAI/2.0/',
            'arxiv': 'http://arxiv.org/OAI/arXiv/'}

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

def unpack_metadata(et):
        md = {}
        m = et.find('oai:GetRecord/oai:record/oai:metadata/arxiv:arXiv', namespaces=prefixes)
        md['authors'] = [ (a.findtext('arxiv:forenames', namespaces=prefixes),
                           a.findtext('arxiv:keyname', namespaces=prefixes))
                         for a in m.findall('arxiv:authors/arxiv:author', namespaces=prefixes) ]
        md['title'] = m.findtext('arxiv:title', namespaces=prefixes)
        md['abstract'] = m.findtext('arxiv:abstract', namespaces=prefixes).strip()
        return md

def get_metadata(arxiv_id):
        query = 'verb=GetRecord&identifier=oai:arXiv.org:%s&metadataPrefix=arXiv' % arxiv_id
        d = oai_request(arxiv_oai_url, query)
        et = ElementTree.fromstring(d)
        md = unpack_metadata(et)
        md['arxiv_id'] = arxiv_id
        return md

if __name__ == '__main__':
        arxiv_id = '0804.2273'
        metad = get_metadata(arxiv_id)
        print metad

