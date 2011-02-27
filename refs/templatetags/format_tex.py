# vim: set fileencoding=UTF-8 :
from django import template
from django.utils.safestring import mark_safe
from django.utils.html import conditional_escape as cond_esc
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

# Format math
sub_re = re.compile(r'([\d\w])_(?:([^\s-]+)[-\s]|\{(.+)\})')
super_re = re.compile(r'([\d\w])\^(?:([^\s-]+)[-\s]|\{(.+)\})')
eq_re = re.compile(r'\$([^\$]+)\$')
def format_math(text):
        text = sub_re.sub(lambda m: '%s<sub>%s</sub> ' %
                          (cond_esc(m.group(1)), cond_esc(m.group(2))), text)
        text = super_re.sub(lambda m: '%s<sup>%s</sup> ' %
                            (cond_esc(m.group(1)), cond_esc(m.group(2))), text)
        text = eq_re.sub(lambda m: '<i>%s</i>' %
                         cond_esc(m.group(1)), text)
        return mark_safe(text)

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
                      lambda m: '<i>%s</i>' % cond_esc(m.group(1)),
                      text)
        return mark_safe(text)

# Do all of the above
@register.filter
def format_tex(text):
        text = format_accents(text)
        text = format_greek(text)
        text = format_style(text)
        text = format_math(text)
        return text

