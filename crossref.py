#!/usr/bin/python

import urllib
import urllib2
import xml.etree.ElementTree

doi_url = 'http://doi.crossref.org/servlet/query?usr=bgamari@physics.umass.edu&pwd=mudpie11&format=xml&id=%s'

def _format_name(given_name, surname):
        return '%s. %s' % (given_name[0], surname)

def _parse_journal_record(journal_rec):
        et = journal_rec
        d = {}
        d['type'] = 'journal_article'
        d['journal'] = et.findtext('journal_metadata/abbrev_title')
        d['volume'] = et.findtext('journal_issue/journal_volume/volume')
        d['issue'] = et.findtext('journal_issue/issue')
        d['title'] = et.findtext('journal_article/titles/title')
        d['year'] = et.findtext('journal_article/publication_date/year')
        authors = et.findall('journal_article/contributors/person_name')
        d['authors'] = ' and '.join(
                [_format_name(a.findtext('given_name')[0], a.findtext('surname'))
                 for a in authors])
        d['pages'] = '%s - %s' % (et.findtext('journal_article/pages/first_page'),
                                  et.findtext('journal_article/pages/last_page'))
        d['doi'] = et.findtext('journal_article/doi_data/doi')

        return d

def lookup_doi(*dois):
        d = map(urllib.quote, dois)
        url = doi_url % '%0A'.join(d)
        f = urllib2.urlopen(url)
        d = f.read()
        print d
        et = xml.etree.ElementTree.fromstring(d)
        j = et.find('doi_record/crossref/journal')
        if j:
                return _parse_journal_record(j)
        else:
                return None

if __name__ == '__main__':
        dois = ['10.1577/H02-043', '10.1021/jp035514+']
        print lookup_doi(*dois)

