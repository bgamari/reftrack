from django.http import HttpResponse, Http404
from django.shortcuts import render_to_response
from django.template import RequestContext
import re

import pymongo
db = pymongo.Connection().reftrack

def index(request):
        return HttpResponse('Hello world!')

def search(request):
        if 'q' not in request.GET:
                return render_to_response('refs/search.html', {},
                                          context_instance=RequestContext(request))

        search = {}
        for w in request.GET['q'].split():
                if w.startswith('author:'):
                        a = re.compile(w.partition(':')[2], re.I)
                        search.setdefault('authors.surname', {'$all': []})['$all'].append(a)

                elif w.startswith('title:'):
                        a = re.compile(w.partition(':')[2], re.I)
                        search.setdefault('title', {'$all': []})['$all'].append(a)

                elif w.startswith('tag:'):
                        a = re.compile(w.partition(':')[2], re.I)
                        search.setdefault('title', {'$all': []})['$all'].append(a)

                else:
                        search.setdefault('keywords', {'$all': []})['$all'].append(w)

        print search
        results = list(db.refs.find(search))
        for ref in results:
                ref['tags'] = db.tags.find({'refs': ref['_id']})
                ref['document'] = db.documents.find_one({'ref': ref['_id']})

        return render_to_response('refs/search.html',
                                  {'refs': results,
                                   'query': request.GET['q']},
                                  context_instance=RequestContext(request))

def show(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.find_one({'_id': ref_id})
        if ref is None: raise Http404
        docs = db.documents.find({'ref': ref_id})
        ref['tags'] = list(db.tags.find({'refs': ref['_id']}))
        return render_to_response('refs/show.html',
                                  {'ref': ref, 'docs': docs},
                                  context_instance=RequestContext(request))

def add_tag(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.find_one({'_id': ref_id})
        if ref is None: raise Http404
        name = request.GET.get('name').strip()
        if name == '' or name is None:
                return HttpResponse('Needs tag', status=500)
        tag = db.tags.find_one({'name': name}) or {'name': name, 'refs': []}
        if ref_id in tag['refs']:
                return HttpResponse('Item already tagged', status=500)
        tag['refs'].append(ref_id)
        db.tags.save(tag)
        return HttpResponse(name)

def rm_tag(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.find_one({'_id': ref_id})
        if ref is None: raise Http404
        name = request.GET.get('name').strip()
        if name == '' or name is None:
                return HttpResponse('Needs tag', status=500)
        tag = db.tags.find_one({'name': name}) or {'name': name, 'refs': []}
        if ref_id not in tag['refs']:
                return HttpResponse('Item does not have tag', status=500)
        tag['refs'].remove(ref_id)
        db.tags.save(tag)
        return HttpResponse(name)

