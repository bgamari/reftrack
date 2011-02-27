from django import template
from django.utils.safestring import mark_safe
from django.utils.html import conditional_escape

register = template.Library()

def format_name(author):
        forenames = author.get('forenames')
        surname = author.get('surname')
        if forenames and surname:
                return '%s. %s' % (conditional_escape(forenames[0]),
                                   conditional_escape(surname))
        elif surname:
                return conditional_escape(surname)
        else:
                return 'Unknown author'

@register.filter
def format_authors(authors, n_authors=None):
        my_authors = authors
        if n_authors is not None:
                my_authors = authors[:n_authors]
        names = [format_name(a) for a in my_authors]
        s = ', '.join(names)
        if n_authors is not None and len(authors) > n_authors:
                s += ', <i>et al.</i>'
        return mark_safe(s)

