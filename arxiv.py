#!/usr/bin/python2.7

import logging
import urllib2
from xml.etree import ElementTree

"""
A simple OAI interface for arXiv metadata access.

Technically the OAI specifications says that implementations may support 
timestamps with second granularity. Unfortunately arXiv does not do this.
"""

arxiv_oai_url = 'http://export.arxiv.org/oai2'

prefixes = {'oai': 'http://www.openarchives.org/OAI/2.0/',
            'arxiv': 'http://arxiv.org/OAI/arXiv/'}

class OAIError(RuntimeError):
        def __init__(self, code, text):
                self.code = code
                self.text = text
        def __str__(self):
                return self.text

class RetryAfterHandler(urllib2.BaseHandler):
        def http_error_503(self, req, fp, code, msg, hdrs):
                from time import sleep
                wait = float(hdrs.get('Retry-After', 60))
                logging.info('Waiting %f seconds' % wait)
                sleep(wait)
                return opener.open(req)

opener = urllib2.build_opener(RetryAfterHandler)

def oai_request(baseurl, **args):
        from time import time
        t = time()
        if 'verb' not in args:
                raise RuntimeError('OAI request needs at least a verb')
        query = '&'.join(('%s=%s' % (k,v) for k,v in args.items()))
        url = '%s?%s' % (baseurl, query)
        logging.debug('Request to %s' % url)
        r = opener.open(url)
        d = r.read()
        with open('last_response.xml', 'w') as f:
                f.write(d)
        logging.debug('OAI request to %s took %f seconds, length %d kB' % (baseurl, time()-t, len(d)/1024))
        et = ElementTree.fromstring(d)
        err = et.find('oai:error', namespaces=prefixes)
        if err is not None:
                logging.error('OAI request failed: %s' % err.text)
                raise OAIError(err.attrib['code'], err.text)
        return et

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
        md['abstract'] = m.findtext('arxiv:abstract', namespaces=prefixes).replace('\n', ' ').strip()
        md['arxiv_categories'] = m.findtext('arxiv:categories', namespaces=prefixes).split()
        md['arxiv_id'] = m.findtext('arxiv:id', namespaces=prefixes)
        md['pub_date'] = parse_date(m.findtext('arxiv:created', namespaces=prefixes))
        md['year'] = md['pub_date'].year
        md['type'] = 'journal_article'
        md['arxiv_journal_ref'] = m.findtext('arxiv:journal-ref', namespaces=prefixes)
        return md

def get_record(oai_url, identifier):
        query = {'verb': 'GetRecord',
                 'identifier': identifier,
                 'metadataPrefix': 'arXiv'}
        et = oai_request(oai_url, **query)
        md = parse_arxiv_record(et.find('oai:GetRecord/oai:record', namespaces=prefixes))
        return md

def lookup_arxiv(arxiv_id):
        md = get_record(arxiv_oai_url, 'oai:arXiv.org:%s' % arxiv_id)
        md['arxiv_id'] = arxiv_id
        return md

def list_records(oai_url, _from=None, _until=None, _set=None):
        query = {'verb': 'ListRecords',
                 'metadataPrefix': 'arXiv'}
        if _from: query['from'] = _from.isoformat()
        if _until: query['until'] = _until.isoformat()
        if _set: query['set'] = _set

        et = oai_request(oai_url, **query)
        resumption_token = et.find('oai:ListRecords/oai:resumptionToken', namespaces=prefixes)
        if resumption_token is not None:
                logging.debug('Got resumption token %s' % resumption_token)
        ets = et.findall('oai:ListRecords/oai:record', namespaces=prefixes)
        records = map(parse_arxiv_record, ets)
        return records, resumption_token
        
def resume_list_records(oai_url, resumption_token):
        query = {'verb': 'ListRecords',
                 'resumptionToken': resumption_token}
        et = oai_request(oai_url, **query)
        resumption_token = et.find('oai:ListRecords/oai:resumptionToken', namespaces=prefixes)
        ets = et.findall('oai:ListRecords/oai:record', namespaces=prefixes)
        records = map(parse_arxiv_record, ets)
        return records, resumption_token

def list_all_records(oai_url, wait=10, **args):
        from time import sleep
        records, resumption_token = list_records(oai_url, **args)
        for r in records: yield r

        while resumption_token is not None:
                sleep(wait)
                r, resumption_token = resume_list_records(oai_url, resumption_token.text)
                for r in records: yield r

def list_sets(oai_url):
        query = {'verb': 'ListSets'}
        et = oai_request(oai_url, **query)
        for s in et.findall('oai:ListSets/oai:set', namespaces=prefixes):
                spec = s.findtext('oai:setSpec', namespaces=prefixes)
                name = s.findtext('oai:setName', namespaces=prefixes)
                yield (spec, name)

if __name__ == '__main__':
        arxiv_id = '0804.2273'
        metad = lookup_arxiv(arxiv_id)
        print metad

