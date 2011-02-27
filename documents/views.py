from django.http import HttpResponse, Http404
import pdf_render
from StringIO import StringIO
import gzip

import pymongo
db = pymongo.Connection().refs

def fetch(request, doc_id):
        doc = db.documents.find_one({'_id': doc_id})
        if doc is None: raise Http404
        resp = HttpResponse(mimetype='application/pdf')
        data = open(doc['filename']).read()
        resp.write(data)
        return resp
        
def render_page(request, doc_id, page_n, format):
        from pdf_render import render_svg
        import os.path
        doc = db.documents.find_one({'_id': doc_id})
        if doc is None: raise Http404
        data = None
        filename = os.path.abspath(doc['filename'])
        print filename
        if format == 'png':
                resp = HttpResponse(mimetype='image/png')
                pdf_render.render_png(resp, filename, int(page_n))
                return resp
        elif format == 'svg':
                resp = HttpResponse(mimetype='image/svg+xml')
                if 'gzip' in request.META['HTTP_ACCEPT_ENCODING']:
                        strio = StringIO()
                        gz = gzip.GzipFile(fileobj=strio, mode='wb')
                        pdf_render.render_svg(gz, filename, int(page_n))
                        gz.close()
                        resp.write(strio.getvalue())
                        resp['Content-Encoding'] = 'gzip'
                else:
                        pdf_render.render_svg(resp, filename, int(page_n))

                return resp
        else:
                raise RuntimeError('Unknown format')

