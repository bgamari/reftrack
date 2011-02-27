import poppler
import cairo
from tempfile import NamedTemporaryFile

def render_svg(dest, fname, page_n, width=700):
        doc = poppler.document_new_from_file('file:///%s' % fname, '')
        if doc.get_n_pages() < page_n:
                raise Http404

        page = doc.get_page(page_n)
        w,h = page.get_size()
        scale = width / w
        surf = cairo.SVGSurface(dest, width, width/w*h)
        cr = cairo.Context(surf)
        cr.set_source_rgb(1,1,1)
        cr.paint()
        cr.scale(scale, scale)
        page.render(cr)
        surf.finish()

def render_png(dest, fname, page_n, scale=1.5):
        doc = poppler.document_new_from_file('file:///%s' % fname, '')
        if doc.get_n_pages() < page_n:
                raise Http404

        page = doc.get_page(page_n)
        w,h = page.get_size()
        surf = cairo.ImageSurface(cairo.FORMAT_RGB24, int(scale*w), int(scale*h))
        cr = cairo.Context(surf)
        cr.scale(scale, scale)
        cr.set_source_rgb(1,1,1)
        cr.paint()
        page.render(cr)
        surf.write_to_png(dest)

def get_n_pages(fname):
        doc = poppler.document_new_from_file('file:///%s' % fname, '')
        return doc.get_n_pages()

