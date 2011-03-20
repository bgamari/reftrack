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
        url = '%sreftrack_refs/_fti/_design/fulltext/all?q=%s&limit=%d&skip=%d' % (db.search_server, urllib.quote(query), limit, skip)
        try:
                print url
                f = urllib2.urlopen(url)
                results = json.load(f)
        except urllib2.HTTPError as e:
                print 'Search request failed: %s' % e
                raise e
                return None

        docs = [(db.Ref.load(db.refs, r['id']), r['score']) for r in results['rows']]
        return docs, results['total_rows']

def find_ref(ref):
        if 'arxiv_id' in ref:
                d = db.refs.get(ref['arxiv_id'])
                if d: return d

        q = ['author:"%s"' % a['surname'] for a in ref['authors']]
        q += ['title:"%s"' % w for w in ref['title'].split()]
        d = run_query(' '.join(q))
        #d = db.refs.find_one({'authors': ref['authors'], 'title': ref['title']})
        if d: return d

        return None

def refs_with_docs():
        refids = [ doc.ref for doc in db.docs.find() ]
        return db.refs.find({'_id': {'$in': refids}})

import pprint
#pprint.pprint(find_ref(db.docs, {'title': 'hidden', 'authors': {'surname': 'talaga'}))
