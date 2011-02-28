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

        query = request.GET['q']
        search = {}
        for w in query.split():
                if w.startswith('author:'):
                        a = re.compile(w.partition(':')[2], re.I)
                        search.setdefault('authors.surname', {'$all': []})['$all'].append(a)

                elif w.startswith('title:'):
                        a = re.compile(w.partition(':')[2], re.I)
                        search.setdefault('title', {'$all': []})['$all'].append(a)

                elif w.startswith('tag:'):
                        a = re.compile(w.partition(':')[2], re.I)
                        search.setdefault('tags.name', {'$all': []})['$all'].append(a)

                else:
                        search.setdefault('keywords', {'$all': []})['$all'].append(w.lower())

        print search
        results = list(db.refs.find(search))
        for ref in results:
                ref['document'] = db.documents.find_one({'ref': ref['_id']})

        show_thumbs = request.GET.get('show_thumbs', '0') != '0'
        return render_to_response('refs/search.html',
                                  {'refs': results,
                                   'query': query,
                                   'show_thumbs': show_thumbs},
                                  context_instance=RequestContext(request))

def show(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.find_one({'_id': ref_id})
        if ref is None: raise Http404
        docs = db.documents.find({'ref': ref_id})
        return render_to_response('refs/show.html',
                                  {'ref': ref, 'docs': docs},
                                  context_instance=RequestContext(request))

def add_tag(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.find_one({'_id': ref_id})
        if ref is None: raise Http404

        name = request.GET.get('name').strip()
        if name == '' or name is None:
                return HttpResponse('Needs tag name', status=500)

        ref_tags = [ tag['name'] for tag in ref.setdefault('tags', []) ]
        if ref_id in ref_tags:
                return HttpResponse('Item already tagged', status=500)

        ref['tags'].append({'name': name})
        db.refs.save(ref)
        return HttpResponse(name)

def rm_tag(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.find_one({'_id': ref_id})
        if ref is None: raise Http404

        name = request.GET.get('name').strip()
        if name == '' or name is None:
                return HttpResponse('Needs tag', status=500)

        ref_tags = [ tag['name'] for tag in ref.setdefault('tags', []) ]
        if name not in ref_tags:
                return HttpResponse('Ref is not tagged', status=500)

        ref['tags'] = [ tag for tag in ref['tags'] if tag['name'] != name ]
        db.refs.save(ref)
        return HttpResponse(name)

