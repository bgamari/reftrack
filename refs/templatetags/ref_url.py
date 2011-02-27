from django import template
import re

register = template.Library()

@register.filter
def ref_url(ref):
        refid = str(ref['_id']).replace('/', '_') # For old arXiv ids
        return '/refs/%s' % refid

@register.filter
def get_id(o):
        return o['_id']

