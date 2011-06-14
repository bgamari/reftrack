#!/usr/bin/python

import couchdb

search_server = 'http://localhost:5984/'
refs = couchdb.Server()['reftrack_refs']
docs = couchdb.Server()['reftrack_documents']

