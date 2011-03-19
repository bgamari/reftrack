#!/usr/bin/python

import database as db

refs_designs = [
        {
                '_id': '_design/reftrack',
                'views': {
                        'by_tags': {'map': 'function(doc){ if ("tags" in doc) doc.tags.forEach(function(t) { emit(t.name, doc); }); }'},
                        'all_tags': {'map': 'function(doc){ if ("tags" in doc) doc.tags.forEach(function(t) { emit(t.name, 1); }); }',
                                     'reduce': 'function(tag, counts){ return sum(counts); }'},
                        'by_author': {'map': 'function(doc){ for (var a in doc.authors) emit(doc.authors[a].surname, doc); }'},
                        'by_pubdate': {'map': 'function(doc){ if ("pub_date" in doc) emit(doc.pub_date, doc); }',
                                       'reduce': 'function(keys, values){ return values.length; }',}
                },
        },
        {
                '_id': '_design/fulltext',
                'fulltext': {
                        'all': {'index': """
function(doc) {
        var ret = new Document();
        ret.add(doc.title, {field: 'default'});
        ret.add(doc.title, {field: 'title'});
        var authors = [];
        for (var a in doc.authors)
                authors.push(doc.authors[a]['surname']);
        ret.add(authors.join(' '), {field: 'author'});
        if ('pub_date' in doc)
                ret.add(new Date(doc.pub_date), {field: 'date'});
        if ('tags' in doc)
                for (var t in doc.tags) ret.add(doc.tags[t].name, {field: 'tag'});
        if ('documents' in doc) {
                ret.attachment('content', doc.documents[0]);
                ret.add('doc document', {field: 'has'});
        }
        return ret;
}
                                """
                        }
                }
        },
]

docs_designs = [
        {
                '_id': '_design/docs',
                'views': {
                        'by_ref': {'map': 'function(doc){ emit(doc.ref, doc); }'},
                },
        },
]

for d in refs_designs:
        doc = db.refs.get(d['_id'], {})
        doc.update(d)
        print db.refs.save(doc)

for d in docs_designs:
        doc = db.docs.get(d['_id'], {})
        doc.update(d)
        print db.docs.save(doc)
