#!/usr/bin/python2.7

import urllib
import urllib2
import xml.etree.ElementTree

username = 'bgamari@physics.umass.edu'
password = 'mudpie11'
doi_url = 'http://doi.crossref.org/servlet/query'

class CrossRefError(RuntimeError):
        def __init__(self, text):
                self.text = text
        def __str__(self):
                return self.text

def _parse_journal_record(journal_rec):
        et = journal_rec
        d = {}
        def set_key(key, val):
                if val: d[key] = val
        d['type'] = 'journal_article'
        d['doi'] = et.findtext('journal_article/doi_data/doi')
        set_key('journal', et.findtext('journal_metadata/abbrev_title'))
        set_key('full_journal', et.findtext('journal_metadata/full_title'))
        set_key('volume', et.findtext('journal_issue/journal_volume/volume'))
        set_key('issue', et.findtext('journal_issue/issue'))
        set_key('title', et.findtext('journal_article/titles/title'))
        set_key('year', et.findtext('journal_article/publication_date/year'))
        authors = et.findall('journal_article/contributors/person_name')
        if authors:
                d['authors'] = [ {'forenames': a.findtext('given_name'),
                                  'surname': a.findtext('surname')} for a in authors]

        first = et.findtext('journal_article/pages/first_page')
        last = et.findtext('journal_article/pages/last_page')
        if first and last:
                d['pages'] = '%s-%s' % (first, last)
        elif first:
                d['pages'] = first
                                  
        return d

def lookup_doi(doi):
        url = '%s?usr=%s&pwd=%s&format=xml&id=%s' % (doi_url, username, password, urllib.quote(doi))
        f = urllib2.urlopen(url)
        d = f.read()
        et = xml.etree.ElementTree.fromstring(d)
        err = et.findtext('doi_record/crossref/error')
        if err is not None:
                raise CrossRefError(err)

        j = et.find('doi_record/crossref/journal')
        if j is not None:
                return _parse_journal_record(j)
        else:
                return None

if __name__ == '__main__':
        import sys
        dois = sys.argv[1:]
        if len(dois) == 0:
                dois = ['10.1577/H02-043', '10.1021/jp035514+']
        for doi in dois:
                print lookup_doi(doi)

