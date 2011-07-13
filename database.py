#!/usr/bin/python

import couchdb
from datetime import datetime

search_server = 'http://localhost:5984/'
server = couchdb.Server()
for db in ['reftrack_refs', 'reftrack_documents']:
        if db not in server: server.create(db)

refs = server['reftrack_refs']
docs = server['reftrack_documents']

def parse_date(str):
        return datetime.strptime(str, '%Y-%m-%d')

def format_date(date):
        return datetime.strftime(date, '%Y-%m-%d')

