from django import template
from django.utils.safestring import mark_safe
from django.utils.html import conditional_escape as cond_esc

register = template.Library()

def format_tag(ref, tag):
        name = cond_esc(tag['name'])
        return '<li class="tag" data-name="%s"></li>' % name

@register.filter
def format_tags(ref):
        print ref
        s = '<ul class="tag-list" data-refid="%s">' % ref['_id'].replace('/', '_')
        s += ' '.join(format_tag(ref, tag) for tag in ref.get('tags', []))
        s += '</ul>'
        return mark_safe(s)

