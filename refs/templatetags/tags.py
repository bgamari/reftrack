from django import template
from django.utils.safestring import mark_safe
from django.utils.html import conditional_escape as cond_esc

register = template.Library()

def format_tag(ref, tag):
        name = cond_esc(tag['name'])
        return '<span class="tag"><a href="/tags/%s">%s</a><a style="display:none;">remove</a></span>' % (name, name)

@register.filter
def format_tags(ref):
        if 'tags' not in ref: return ''
        s = ', '.join(format_tag(ref, tag) for tag in ref['tags'])
        s += '<a>Add</a>'
        return mark_safe(s)

