#!/usr/bin/python2.7

import logging
import urllib2
from xml.etree import ElementTree

arxiv_oai_url = 'http://export.arxiv.org/oai2'

prefixes = {'oai': 'http://www.openarchives.org/OAI/2.0/',
            'arxiv': 'http://arxiv.org/OAI/arXiv/'}

class RetryAfterHandler(urllib2.BaseHandler):
        def http_error_503(self, req, fp, code, msg, hdrs):
                from time import sleep
                wait = float(hdrs.get('Retry-After', 60))
                logging.info('Waiting %f seconds' % wait)
                sleep(wait)
                return opener.urlopen(req)

opener = urllib2.build_opener(RetryAfterHandler)

def oai_request(url, query):
        from time import time
        t = time()
        r = opener.open('%s?%s' % (url, query))
        d = r.read()
        logging.debug('OAI request to %s took %f seconds, length %d kB' % (url, time()-t, len(d)/1024))
        return d

def parse_date(str):
        from datetime import datetime
        return datetime.strptime(str, '%Y-%m-%d')

def parse_arxiv_record(et):
        md = {}
        m = et.find('oai:metadata/arxiv:arXiv', namespaces=prefixes)
        md['authors'] = [
                {'forenames': a.findtext('arxiv:forenames', namespaces=prefixes),
                 'surname': a.findtext('arxiv:keyname', namespaces=prefixes)}
                 for a in m.findall('arxiv:authors/arxiv:author', namespaces=prefixes)
        ]
        md['title'] = m.findtext('arxiv:title', namespaces=prefixes)
        md['abstract'] = m.findtext('arxiv:abstract', namespaces=prefixes).strip()
        md['arxiv_categories'] = m.findtext('arxiv:categories', namespaces=prefixes).split()
        md['arxiv_id'] = m.findtext('arxiv:id', namespaces=prefixes)
        md['pub_date'] = parse_date(m.findtext('arxiv:created', namespaces=prefixes))
        md['type'] = 'journal_article'
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
        with open('last_response.xml', 'w') as f:
                f.write(d)
        et = ElementTree.fromstring(d)
        resumption_token = et.find('oai:ListRecords/oai:resumptionToken', namespaces=prefixes)
        if resumption_token is not None:
                logging.debug('Got resumption token %s' % resumption_token)
        ets = et.findall('oai:ListRecords/oai:record', namespaces=prefixes)
        records = map(parse_arxiv_record, ets)
        return records, resumption_token
        
def resume_list_records(oai_url, resumption_token):
        query = 'verb=ListRecords&resumptionToken=%s' % resumption_token
        d = oai_request(oai_url, query)
        et = ElementTree.fromstring(d)
        resumption_token = et.find('oai:ListRecords/oai:resumptionToken', namespaces=prefixes)
        ets = et.findall('oai:ListRecords/oai:record', namespaces=prefixes)
        records = map(parse_arxiv_record, ets)
        return records, resumption_token

def list_all_records(oai_url, wait=10, **args):
        from time import sleep
        records, resumption_token = list_records(oai_url, **args)
        for r in records:
                yield r

        while resumption_token is not None:
                sleep(wait)
                r, resumption_token = resume_list_records(oai_url, resumption_token.text)
                for r in records:
                        yield r

if __name__ == '__main__':
        arxiv_id = '0804.2273'
        #metad = lookup_arxiv(arxiv_id)
        #print metad
        
        from datetime import date, timedelta
        _from = date.today() - timedelta(days=1)
        _until = date.today()
        list_records(arxiv_oai_url, _set='physics', _from=_from, _until=_until)

