import poppler
import cairo
from tempfile import NamedTemporaryFile

def render_svg(fname, page_n, width=100, height=100):
        doc = poppler.document_new_from_file(fname)
        if doc.get_n_pages() < page_n:
                raise Http404

        tmp = NamedTemporaryFile()
        surf = cairo.SVGSurface(tmp, width, height)
        cr = cairo.Context(surf)
        doc.get_page(page_n).render(cr)
        surf.finish()
        return open(temp, 'r').read()

def render_png(fname, page_n, width=100, height=100):
        doc = poppler.document_new_from_file(fname)
        if doc.get_n_pages() < page_n:
                raise Http404

        tmp = NamedTemporaryFile()
        surf = cairo.PNGSurface(tmp, width, height)
        cr = cairo.Context(surf)
        doc.get_page(page_n).render(cr)
        surf.finish()
        return open(temp, 'r').read()

