#!/usr/bin/python

import urllib2
import xml.etree.ElementTree

username = 'bgamari@physics.umass.edu'
passwd = 'mudpie11'
doi_url = 'http://doi.crossref.org/servlet/deposit'

def _build_query(queries):
        b = xml.etree.ElementTree.TreeBuilder()
        b.start('query_batch', {'version': '2.0',
                                'xmlns': 'http://www.crossref.org/qschema/2.0',
                                'xmlns:xsi': 'http://www.w3.org/2001/XMLSchema-instance'})
        b.start('head', {})

        b.start('email_address', {})
        b.data('bgamari@physics.umass.edu')
        b.end('email_address')

        b.start('doi_batch_id', {})
        b.data('hello world')
        b.end('doi_batch_id')

        b.end('head')
        
        b.start('body', {})
        for i, q in enumerate(queries):
                b.start('query', {'key': 'query%d' % i})
                for k,v in q.items():
                        b.start(k, {})
                        b.data(v)
                        b.end(k)
                b.end('query')

        b.end('body')
        b.end('query_batch')
        q = b.close()
        return xml.etree.ElementTree.tostring(q)

def lookup_doi(doi):
        url = '%s?operation=doDOIQueryUpload&login_id=%s&login_passwd=%s' % (doi_url, username, passwd)
        headers = {
                'Content-Disposition': 'form-data; name="fname"; filename="crossref-query.xml"',
                'Content-Type': 'multipart/form-data',
        }
        q = [{'doi': doi}]
        query = _build_query(q)
        print query
        req = urllib2.Request(doi_url, query, headers)
        resp = urllib2.urlopen(req)
        d = resp.read()
        print d
        et = xml.etree.ElementTree.fromstring(d)

if __name__ == '__main__':
        doi = '10.1577/H02-043'
        print lookup_doi(doi)

