from django.http import HttpResponse, Http404
from django.shortcuts import render_to_response
from django.template import RequestContext
import re

import pymongo
db = pymongo.Connection().refs

def index(request):
        return HttpResponse('Hello world!')

def search(request):
        if request.method == 'POST':
                search = {}
                if len(request.POST.get('author', '')) > 0:
                        for word in request.POST['author']:
                                author_re = re.compile(word, re.I)
                                search['$or'] = [{'authors.surname': author_re},
                                                 {'authors.forenames': author_re}]

                if len(request.POST.get('title', '')) > 0:
                        for word in request.POST['title'].split():
                                search['title'] = re.compile(word, re.I)

                if len(request.POST.get('keywords', '')) > 0:
                        for word in request.POST['keywords'].split():
                                search['keywords'] = re.compile(word, re.I)

                print search
                results = db.refs.find(search)
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

