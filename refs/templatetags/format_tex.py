# vim: set fileencoding=UTF-8 :
from django import template
from django.utils.safestring import mark_safe
from django.utils.html import conditional_escape
import re

register = template.Library()

# Format accents
accents = {
        '\`a': u'á',
}

def format_accents(text):
        for k,v in accents.items():
                text = text.replace(k,v)
        return text

# Translate Greek characters
chars = {
        'alpha': u'α',          'beta': u'β',                   'gamma': u'γ',
        'delta': u'δ',          'epsilon': u'ε',                'zeta': u'ζ',
}

greek_res = [
        re.compile(r'\$\\(\w+)\$'),
        re.compile(r'\{\\(\w+)\}'),
        re.compile(r'\\(\w+)'),
]

def format_greek(text):
        def repl(match):
                char = match.group(1)
                return chars.get(char, '\%s' % char)
        for r in greek_res:
                text = r.sub(repl, text)
        return text

# Format style commands
def format_style(text):
        text = re.sub(r'\{\s*\\it(.+)\}',
                      lambda m: '<i>%s</i>' % conditional_escape(m.group(1)),
                      text)
        return mark_safe(text)

# Do all of the above
@register.filter
def format_tex(text):
        text = format_accents(text)
        text = format_greek(text)
        text = format_style(text)
        return text

