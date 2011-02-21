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

def parse_arxiv_record(et):
        md = {}
        m = et.find('oai:metadata/arxiv:arXiv', namespaces=prefixes)
        md['authors'] = [ (a.findtext('arxiv:forenames', namespaces=prefixes),
                           a.findtext('arxiv:keyname', namespaces=prefixes))
                         for a in m.findall('arxiv:authors/arxiv:author', namespaces=prefixes) ]
        md['title'] = m.findtext('arxiv:title', namespaces=prefixes)
        md['abstract'] = m.findtext('arxiv:abstract', namespaces=prefixes).strip()
        return md

def get_record(oai_url, identifier):
        query = 'verb=GetRecord&identifier=%s&metadataPrefix=arXiv' % identifier
        d = oai_request(oai_url, query)
        et = ElementTree.fromstring(d)
        md = parse_archiv_record(et.find('oai:GetRecord/oai:record', namespaces=prefixes))
        md['arxiv_id'] = arxiv_id
        return md

def lookup_arxiv(arxiv_id):
        return get_record(arxiv_oai_url, 'oai:arXiv.org:%s' % arxiv_id)

def list_records(oai_url, _from=None, _until=None, _set=None):
        query = 'verb=ListRecords&metadataPrefix=arXiv'
        if _from:
                query += '&from=%s' % _from.isoformat()
        if _until:
                query += '&until=%s' % _until.isoformat()
        if _set:
                query += '&set=%s' % _set
        d = oai_request(oai_url, query)
        et = ElementTree.fromstring(d)
        ets = et.findall('oai:ListRecords/oai:record', namespaces=prefixes)
        records = map(parse_arxiv_record, ets)
        return records
        
def resume_list_records(resumption_token):
        query = 'verb=ListRecords&resumptionToken=%s' % resumption_token
        d = oai_request(oai_url, query)
        et = ElementTree.fromstring(d)
        ets = et.findall('oai:ListRecords/oai:record', namespaces=prefixes)
        records = map(parse_arxiv_record, ets)
        return records

if __name__ == '__main__':
        arxiv_id = '0804.2273'
        #metad = lookup_arxiv(arxiv_id)
        #print metad
        
        from datetime import date, timedelta
        _from = date.today() - timedelta(days=1)
        _until = date.today()
        list_records(arxiv_oai_url, _set='physics', _from=_from, _until=_until)

