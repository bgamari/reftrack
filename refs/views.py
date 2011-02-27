from django.http import HttpResponse, Http404
from django.shortcuts import render_to_response
from django.template import RequestContext
import re

import pymongo
db = pymongo.Connection().reftrack

def index(request):
        return HttpResponse('Hello world!')

def search(request):
        if request.method == 'POST':
                search = {}
                if len(request.POST.get('author', '')) > 0:
                        res = [re.compile(word, re.I) for word in request.POST['author'].split()]
                        search['authors.surname'] = {'$all': res}

                if len(request.POST.get('title', '')) > 0:
                        res = [re.compile(word, re.I) for word in request.POST['title'].split()]
                        search['title'] = {'$all': res}

                if len(request.POST.get('keywords', '')) > 0:
                        res = [re.compile(word, re.I) for word in request.POST['keywords'].split()]
                        search['keywords'] = {'$all': res}

                print search
                results = list(db.refs.find(search))
                for ref in results:
                        ref['tags'] = db.tags.find({'ref': ref['_id']})
                return render_to_response('refs/results.html', {'refs': results},
                                          context_instance=RequestContext(request))
        else:
                return render_to_response('refs/search.html', {},
                                          context_instance=RequestContext(request))

def show(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.find_one({'_id': ref_id})
        if ref is None: raise Http404
        docs = db.documents.find({'ref': ref_id})
        return render_to_response('refs/show.html',
                                  {'ref': ref, 'docs': docs},
                                  context_instance=RequestContext(request))

