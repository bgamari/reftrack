from django.http import HttpResponse, Http404
from django.shortcuts import render_to_response
from django.template import RequestContext
from json_response import JSONResponse
import re
import logging
import json

from index import fulltext_query
import database as db

results_per_page = 25

def index(request):
        return HttpResponse('Hello world!')

def search(request):
        from math import ceil
        if 'q' not in request.GET:
                return render_to_response('refs/search.html', {},
                                          context_instance=RequestContext(request))

        query = request.GET['q']
        page = int(request.GET.get('page', 1))
        skip = (page-1) * results_per_page
        results, total_rows = fulltext_query(query, skip=skip, limit=results_per_page)
        results = [res for res,score in results]
        npages = ceil(1. * total_rows / results_per_page)
        show_thumbs = request.GET.get('show_thumbs', '0') != '0'
        return render_to_response('refs/search.html',
                                  {'refs': results,
                                   'page': page,
                                   'n_pages': npages,
                                   'n_results': total_rows,
                                   'pages': range(1, npages+1),
                                   'query': query,
                                   'show_thumbs': show_thumbs},
                                  context_instance=RequestContext(request))

def search_results(request):
        from math import ceil
        query = request.GET['q']
        page = int(request.GET.get('page', 1))
        skip = (page-1) * results_per_page
        results, total_rows = fulltext_query(query, skip=skip, limit=results_per_page)
        results = [res for res,score in results]
        npages = ceil(1. * total_rows / results_per_page)
        show_thumbs = request.GET.get('show_thumbs', '0') != '0'
        return render_to_response('refs/search_results.html',
                                  {'refs': results,
                                   'page': page,
                                   'n_pages': npages,
                                   'n_results': total_rows,
                                   'pages': range(1, npages+1),
                                   'query': query,
                                   'show_thumbs': False},
                                  context_instance=RequestContext(request))

def show(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.Ref.load(db.refs, ref_id)
        if ref is None: raise Http404
        docs = db.docs.view('docs/by_ref', key=ref_id)
        return render_to_response('refs/show.html',
                                  {'ref': ref, 'docs': docs},
                                  context_instance=RequestContext(request))

def add_tag(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.get(ref_id)
        if ref is None: raise Http404
        if not request.POST: raise Http404

        name = request.POST.get('name').strip()
        if name == '' or name is None:
                return HttpResponse('Needs tag name', status=500)

        ref_tags = [ tag['name'] for tag in ref.setdefault('tags', []) ]
        if ref_id in ref_tags:
                return HttpResponse('Item already tagged', status=500)

        ref['tags'].append({'name': name})
        db.refs.save(ref)
        return HttpResponse(name)

def rm_tag(request, ref_id):
        ref_id = ref_id.replace('_', '/')
        ref = db.refs.get(ref_id)
        if ref is None: raise Http404
        if request.POST == None: raise Http404

        print request.POST
        name = request.POST.get('name').strip()
        if name == '' or name is None:
                return HttpResponse('Needs tag', status=500)

        ref_tags = [ tag['name'] for tag in ref.setdefault('tags', []) ]
        if name not in ref_tags:
                return HttpResponse('Ref is not tagged', status=500)

        ref['tags'] = [ tag for tag in ref['tags'] if tag['name'] != name ]
        db.refs.save(ref)
        return HttpResponse(name)

def tag_list(request):
        tags = dict((r.key, r.value) for r in db.refs.view('reftrack/all_tags', group=True))
        return render_to_response('refs/tags.html',
                                  {'tags': tags},
                                  context_instance=RequestContext(request))

def bulk_add_tag(request):
        if not request.POST:
                return HttpResponse('Must be called with POST request', status=500)
        refs = json.loads(request.POST['refs'])
        tag = request.POST['tag']
        if not tag:
                return HttpResponse('Must be called with tag parameter', status=500)

        succeeded = []
        for ref_id in refs:
                ref = db.refs.get(ref_id)
                if ref and {'name': tag} not in ref['tags']:
                        ref['tags'].append({'name': tag})
                        succeeded.append(ref_id)
                        db.refs.save(ref)
                elif not ref:
                        logging.warn('Ref %s not found' % ref_id)
                else:
                        logging.info('Ref %s already tagged with %s' % (ref_id, tag))
        
        return JSONResponse(succeeded)

def bulk_remove_tag(request):
        if not request.POST:
                return HttpResponse('Must be called with POST request', status=500)
        refs = json.loads(request.POST['refs'])
        tag = request.POST['tag']
        if not tag:
                return HttpResponse('Must be called with tag parameter', status=500)

        succeeded = []
        for ref_id in refs:
                ref = db.refs.get(ref_id)
                if ref and {'name': tag} in ref['tags']:
                        ref['tags'].remove({'name': tag})
                        succeeded.append(ref_id)
                        db.refs.save(ref)
                elif not ref:
                        logging.warn('Ref %s not found' % ref_id)
                else:
                        logging.info('Ref %s not tagged with %s' % (ref_id, tag))
        
        return JSONResponse(succeeded)

def tag_search(query):
        import json, urllib2
        import urllib
        url = '%sreftrack_refs/_fti/_design/fulltext/tags?q=%s' % (db.search_server, urllib.quote(query))
        try:
                print url
                f = urllib2.urlopen(url)
                results = json.load(f)
        except urllib2.HTTPError as e:
                print 'Search request failed: %s' % e
                raise e
                return None

        docs = [(db.Ref.load(db.refs, r['id']), r['score']) for r in results['rows']]
        return docs, results['total_rows']

def tag_suggestions(request):
        query = request.GET['q']

