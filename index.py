#!/usr/bin/python2.7

import pymongo

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

def find_ref(db, ref):
        if 'arxiv_id' in ref:
                d = db.refs.find_one({'arxiv_id': ref['arxiv_id']})
                if d: return d

        d = db.refs.find_one({'authors': ref['authors'], 'title': ref['title']})
        if d: return d

        return None

