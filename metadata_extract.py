#!/usr/bin/python

import sys
import re
doi_re = re.compile(r'(?:doi|DOI)?[\s\.\:]{0,3}(10\.\d{4}/[\d\w\-\.\+]+)')
arxiv_re = re.compile(r'arXiv:(\d{4,4}\.\d{4,4})')

def find_metadata(file):
        metadata = {}
        p = subprocess.Popen(['pdftotext', file, '-'], stdout=subprocess.PIPE)
        txt = p.stdout.read()

        m = doi_re.search(txt)
        if m:
                metadata['doi'] = m.group(1)

        m = arxiv_re.search(txt)
        if m:
                metadata['arvix_id'] = m.group(1)

        return metadata
        
def find_doi_pdfminer(file):
        from pdfminer.converter import PDFPageAggregator
        from pdfminer.layout import LAParams, LTTextBox, LTTextLine
        from pdfminer.pdfparser import PDFDocument, PDFParser
        from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter

        rsrc = PDFResourceManager()
        device = PDFPageAggregator(rsrc, laparams=LAParams())
        doc = PDFDocument()
        fp = open(file)
        parser = PDFParser(fp)
        parser.set_document(doc)
        doc.set_parser(parser)
        doc.initialize('')
        interpreter = PDFPageInterpreter(rsrc, device)

        pages = list(doc.get_pages())
        interpreter.process_page(pages[0])
        layout = device.get_result()

        print sys.argv[1], layout.__dict__
        for n,lt_obj in enumerate(layout._objs):
                if isinstance(lt_obj, LTTextBox):
                        m = doi_re.search(lt_obj.text)
                        if m:
                                print m.group(1),
                        height = lt_obj._objs[0].height
                        print 'TextBox (%f): %s' % (height, lt_obj.text.encode('iso-8859-1', 'ignore'))
                
                if isinstance(lt_obj, LTTextLine):
                        print 'TextLine: %s (%f)' % (lt_obj.text.encode('iso-8859-1'), lt_obj.height)

                if n == 5: break

        print

if __name__ == '__main__':
        import sys
        for f in sys.argv[1:]:
                print f, find_doi(f)

