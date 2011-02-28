from StringIO import StringIO
import gzip
import os.path

from django.shortcuts import render_to_response
from django.http import HttpResponse, Http404
from django.template import RequestContext
import pymongo
db = pymongo.Connection().reftrack

import pdf_render

def fetch(request, doc_id):
        doc = db.documents.find_one({'_id': doc_id})
        if doc is None: raise Http404
        resp = HttpResponse(mimetype='application/pdf')
        data = open(doc['filename']).read()
        resp.write(data)
        return resp
        
def render_page(request, doc_id, page_n):
        doc = db.documents.find_one({'_id': doc_id})
        if doc is None: raise Http404
        format = request.GET.get('format', 'svg')
        if format not in ['svg', 'png']:
                return HttpResponse('Invalid format', status=500)
        data = None
        filename = os.path.abspath(doc['filename'])
        width = float(request.GET.get('width', 700))
        if format == 'png':
                resp = HttpResponse(mimetype='image/png')
                pdf_render.render_png(resp, filename, int(page_n), width)
                return resp
        elif format == 'svg':
                resp = HttpResponse(mimetype='image/svg+xml')
                if 'gzip' in request.META['HTTP_ACCEPT_ENCODING']:
                        strio = StringIO()
                        gz = gzip.GzipFile(fileobj=strio, mode='wb')
                        pdf_render.render_svg(gz, filename, int(page_n), width)
                        gz.close()
                        resp.write(strio.getvalue())
                        resp['Content-Encoding'] = 'gzip'
                else:
                        pdf_render.render_svg(resp, filename, int(page_n))

                return resp
        else:
                raise RuntimeError('Unknown format')

def view(request, doc_id, page_n=1):
        doc = db.documents.find_one({'_id': doc_id})
        if doc is None: raise Http404
        ref = db.refs.find_one({'_id': doc['ref']})
        filename = os.path.abspath(doc['filename'])
        n_pages = pdf_render.get_n_pages(filename)

        format = request.GET.get('format', 'svg')
        args = {'ref': ref, 'doc': doc,
                'page_n': page_n, 'n_pages': n_pages,
                'format': format}
        if format == 'svg':
                width = float(request.GET.get('width', 700))
                args['width'] = width
        elif format == 'png':
                width = float(request.GET.get('width', 700))
                if width > 10000:
                        return HttpResponse('Requested size too large', status=500)
                args['width'] = width
        else:
                return HttpResponse('Invalid format', status=500)

        return render_to_response('documents/view.html', args,
                                  context_instance=RequestContext(request))

