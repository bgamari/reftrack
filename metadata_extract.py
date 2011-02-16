#!/usr/bin/python

import sys
from pdfminer.converter import PDFPageAggregator
from pdfminer.layout import LAParams, LTTextBox, LTTextLine
from pdfminer.pdfparser import PDFDocument, PDFParser
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter

import re
doi_re = re.compile(r'(?:doi|DOI)?[\s\.\:]{0,3}(10\.\d{4}/[\d\w\-\.\+]+)')
doi = '10.1021/jp035514+'
print doi_re.search(doi)


rsrc = PDFResourceManager()
device = PDFPageAggregator(rsrc, laparams=LAParams())
doc = PDFDocument()
fp = open(sys.argv[1])
parser = PDFParser(fp)
parser.set_document(doc)
doc.set_parser(parser)
doc.initialize('')
interpreter = PDFPageInterpreter(rsrc, device)

pages = list(doc.get_pages())
interpreter.process_page(pages[0])
layout = device.get_result()

print sys.argv[1],
print layout.__dict__
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
