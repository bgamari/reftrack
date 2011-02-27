import subprocess
from django.http import HttpResponse
from django.shortcuts import render_to_response
from django.template import RequestContext
from django.http import Http404
import re

import pymongo
db = pymongo.Connection().refs

def index(request):
        return HttpResponse('Hello world!')

def search(request):
        if request.method == 'POST':
                search = {}
                if 'author' in request.POST and len(request.POST['author']) > 0:
                        author_re = re.compile(request.POST['author'], re.I)
                        search['$or'] = [{'authors.surname': author_re},
                                         {'authors.forenames': author_re}]

                if 'title' in request.POST and len(request.POST['title']) > 0:
                        search['title'] = re.compile(request.POST['title'])

                if 'keywords' in request.POST and len(request.POST['keywords']) > 0:
                        for word in request.POST['keywords'].split():
                                word_re = re.compile(word, re.I)
                                search['$or'] = [{'authors.surname': word_re},
                                                 {'authors.forenames': word_re},
                                                 {'title': word_re},]

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
        return render_to_response('refs/show.html', {'ref': ref},
                                  context_instance=RequestContext(request))

def render_page(request, ref_id, page_n, format):
        from pdf_render import render_svg
        file = db.files.find_one({'ref': ref_id})
        if file is None: raise Http404
        data = None
        print '"%s"' % format
        if format == 'png':
                data = render_png(file['filename'], int(page_n))
        elif format == 'svg':
                data = render_svg(file['filename'], int(page_n))
        resp = HttpResponse()
        resp.write(data)
        return resp
