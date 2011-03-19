#!/usr/bin/python

import couchdb

search_server = 'http://localhost:5984/'
refs = couchdb.Server()['reftrack_refs']
docs = couchdb.Server()['reftrack_documents']

from couchdb.mapping import *
class Ref(Document):
        title = TextField()
        authors = ListField(Mapping.build(forenames = TextField(), surname = TextField()))
        arxiv_categories = ListField(TextField())
        arxiv_id = TextField()
        pub_date = DateField()
        type = TextField()
        tags = ListField(Mapping.build(name = TextField()))

class Doc(Document):
        ref = TextField()
        md5 = TextField()
        filename = TextField()
