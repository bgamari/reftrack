from django.http import HttpResponse, Http404
from django.shortcuts import render_to_response
from django.template import RequestContext
import re

import pymongo
db = pymongo.Connection().reftrack

def index(request):
        return HttpResponse('Hello world!')

def search(request):
        from itertools import chain
        if 'q' not in request.GET:
                return render_to_response('refs/search.html', {},
                                          context_instance=RequestContext(request))

        query = request.GET['q']
        search = {}

        # First look for unqualified terms (e.g. hello)
        for m in re.finditer(r'(\s|\A)([^\s:"]+)(\s|\Z)', query):
                search.setdefault('keywords', {'$all': []})['$all'].append(m.group(2).lower())

        # Look for qualified terms (e.g. tag:"hello world")
        qual_terms = re.finditer(r'(\w+):([^"\s]+)', query)
        quoted_qual_terms = re.finditer(r'(\w+):"([^"]+)"', query)
        for m in chain(qual_terms, quoted_qual_terms):
                qual = m.group(1) # e.g. 'tag'
                term = m.group(2) # e.g. 'hello world'
                a = re.compile('^%s$' % term, re.I)

                if qual == 'author':
                        search.setdefault('authors.surname', {'$all': []})['$all'].append(a)

                elif qual == 'title':
                        search.setdefault('title', {'$all': []})['$all'].append(a)

                elif qual == 'tag':
                        search.setdefault('tags.name', {'$all': []})['$all'].append(term)

                elif qual == 'with':
                        if term in ['document', 'doc']:
                                search['documents'] = {'$exists': True}

        if search == {}:
                return render_to_response('refs/search.html', {'query': query},
                                          context_instance=RequestContext(request))

        print search
        results = list(db.refs.find(search, limit=1000))
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

        name = request.POST.get('name').strip()
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

        name = request.POST.get('name').strip()
        if name == '' or name is None:
                return HttpResponse('Needs tag', status=500)

        ref_tags = [ tag['name'] for tag in ref.setdefault('tags', []) ]
        if name not in ref_tags:
                return HttpResponse('Ref is not tagged', status=500)

        ref['tags'] = [ tag for tag in ref['tags'] if tag['name'] != name ]
        db.refs.save(ref)
        return HttpResponse(name)

def tags_list(request):
        tags = []
        return render_to_response('refs/tags.html',
                                  {'tags': tags},
                                  context_instance=RequestContext(request))
