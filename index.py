#!/usr/bin/python2.7

import database as db

def update_documents():
        for ref in db.refs.view('_all_docs'):
                print ref
                docs = db.docs.view('docs/by_ref', key=ref.id)
                ref['documents'] = [d.id for d in docs]
                print ref['documents']

def merge_refs(p1, p2):
        d1 = dict([ (p['md5'], p) for p in p1 ])
        d2 = dict([ (p['md5'], p) for p in p2 ])

        res = []
        for k in d1.keys()+d2.keys():
                r = {}
                r.update(d1.get(k, {}))
                r.update(d2.get(k, {}))
                res.append(r)

        return res

def fulltext_query(query, limit=25, skip=0):
        import json, urllib2
        import urllib
        url = '%sreftrack_refs/_fti/_design/fulltext/all?q=%s&limit=%d&skip=%d' % \
                        (db.search_server,
                         urllib.quote(query.encode('utf-8')),
                         limit, skip)
        try:
                f = urllib2.urlopen(url)
                results = json.load(f)
        except urllib2.HTTPError as e:
                print 'Search request failed: %s' % e
                raise e
                return None

        docs = [(db.refs.get(r['id']), r['score']) for r in results['rows']]
        return docs, results['total_rows']

def find_ref(ref):
        if 'arxiv_id' in ref:
                d = db.refs.get(ref['arxiv_id'])
                if d: return d

        clean_title = filter(lambda s: s.isalpha(), ref['title'].split())
        q = ['author:"%s"' % a['surname'] for a in ref['authors']]
        q += ['title:"%s"' % a for a in clean_title]
        docs, rows = fulltext_query(' '.join(q))
        #d = db.refs.find_one({'authors': ref['authors'], 'title': ref['title']})
        if rows > 0: return docs[0][0]
        return None

def refs_with_docs():
        refids = [ doc.ref for doc in db.docs.find() ]
        return db.refs.find({'_id': {'$in': refids}})

import pprint
#pprint.pprint(find_ref(db.docs, {'title': 'hidden', 'authors': {'surname': 'talaga'}))
