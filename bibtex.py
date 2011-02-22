#!/usr/bin/python2.7

def format_bibtex(ref, refname):
        types = {
                'journal_article': '@article',
        }
        values = {
                'journal': 'journal',
                'volume': 'volume',
                'issue': 'issue',
                'year': 'year',
                'title': 'title',
        }

        if ref['type'] not in types:
                raise RuntimeError('Unknown reference type')
        s = '%s { %s \n' % (types[ref['type'], refname)
        for key,mykey in values.items():
                if mykey in ref:
                        s += '%s = {%s},\n' % (key, ref[mykey])

        if 'authors' in ref:
                authorlist = ' and '.join(['%s. %s' % (given[0], surname)
                                           for given, surname in ref['authors']])
                s += 'author = {%s}\n' % authorlist

        s += '}\n'
        return s

