#!/usr/bin/python2.7

import sys
import re
import subprocess
from collections import namedtuple

doi_re = re.compile(r'(?:doi|DOI)?[\s\.\:/]{0,3}(10\.\d{4}/[\(\)\d\w\-\.\+/]+)', re.I)
pnas_doi_re = re.compile(r'doi(10\.1073pnas.\d+)', re.I)
arxiv_re = re.compile(r'arXiv:(\d{4,4}\.\d{4,4})(v\d+)?')

def find_ids(file):
        metadata = {}
        txt = subprocess.check_output(['pdftotext', file, '-'])

        m = doi_re.search(txt)
        if m:
                metadata['doi'] = m.group(1).rstrip('.') # Some journals end sentence with doi

        m = arxiv_re.search(txt)
        if m:
                metadata['arxiv_id'] = m.group(1)

        return metadata

def flatten_text(el):
        """ Returns the concatenated text of all children of element el """
        return ''.join(el.itertext())

def find_metadata(file):
        from xml.etree import ElementTree
        metadata = {}
        s = subprocess.check_output(['pdftohtml', '-xml', '-stdout', file])
        if len(s) == 0:
                logging.warn('Failed to extract PDF content')
                raise RuntimeError('Failed to extract PDF content')
        et = ElementTree.fromstring(s)
        first_page = et.find('page')
        page_h, page_w = first_page.attrib['height'], first_page.attrib['width']

        # Map font ids to sizes
        font_ids = { fs.attrib['id']: float(fs.attrib['size']) for fs in first_page.findall('fontspec') }
        found_fonts = [ el.attrib['font'] for el in first_page.findall('text') ]
        # List of font ids found on first page sorted by size
        sorted_font_ids = sorted(found_fonts, key=font_ids.get, reverse=True)

        # To find the title we look for largest text on first page.  When we
        # find multiple lines we check to see if they are a single line by
        # comparing the difference in their y positions to their height
        largest_text = first_page.findall('text[@font="%s"]' % sorted_font_ids[0])
        title = ''
        last_y = float(largest_text[0].attrib['top'])
        for t in largest_text:
                y = float(t.attrib['top'])
                height = float(t.attrib['height'])
                if abs(last_y - y) / height > 1.5:
                        break
                title += ' ' + flatten_text(t)
                last_y = y

        # Find authors: Look for second largest text on page
        text = first_page.findall('text[@font="%s"]' % sorted_font_ids[1])
        print map(flatten_text, text)

        return title

if __name__ == '__main__':
        import sys
        for f in sys.argv[1:]:
                print f, find_metadata(f)

