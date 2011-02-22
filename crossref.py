#!/usr/bin/python

import urllib
import urllib2
import xml.etree.ElementTree

doi_url = 'http://doi.crossref.org/servlet/query?usr=bgamari@physics.umass.edu&pwd=mudpie11&format=xml&id=%s'

def _parse_journal_record(journal_rec):
        et = journal_rec
        d = {}
        def set_key(key, val):
                if val: d[key] = val
        d['type'] = 'journal_article'
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
                                  
        d['doi'] = et.findtext('journal_article/doi_data/doi')

        return d

def lookup_doi(*dois):
        d = map(urllib.quote, dois)
        url = doi_url % '%0A'.join(d)
        f = urllib2.urlopen(url)
        d = f.read()
        et = xml.etree.ElementTree.fromstring(d)
        j = et.find('doi_record/crossref/journal')
        if j:
                return _parse_journal_record(j)
        else:
                return None

if __name__ == '__main__':
        import sys
        dois = sys.argv[1:]
        if len(dois) == 0:
                dois = ['10.1577/H02-043', '10.1021/jp035514+']
        print lookup_doi(*dois)

