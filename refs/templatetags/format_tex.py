# vim: set fileencoding=UTF-8 :
from django import template
from django.utils.safestring import mark_safe
from django.utils.html import conditional_escape as cond_esc
import re

register = template.Library()

# Format accents
accents = {
        r"\'a": u"á",           r"\'e": u"é",           r"\'i": u"í",
        r"\'o": u"ó",           r"\'u": u"ú",

        r"\`a": u"à",           r"\`e": u"è",           r"\`i": u"ì",
        r"\`o": u"ò",           r"\`u": u"ù",

        r'\"a': u'ä',           r'\"e': u'ë',           r'\"i': u'ï',
        r'\"o': u'ö',           r'\"u': u'ü',
}

def format_accents(text):
        for k,v in accents.items():
                text = text.replace(k,v)
        return text

# Format math
def format_math(text):
        text = re.sub(r'([\d\w])_\{(.+)\}',
                      lambda m: '%s<sub>%s</sub> ' % (cond_esc(m.group(1)), cond_esc(m.group(2))), text)
        text = re.sub(r'([\d\w])_(\-?[^\s\-\(\)]+)[-\s]',
                      lambda m: '%s<sub>%s</sub> ' % (cond_esc(m.group(1)), cond_esc(m.group(2))), text)
        text = re.sub(r'([\d\w])\^\{(.+)\}',
                      lambda m: '%s<sup>%s</sup> ' % (cond_esc(m.group(1)), cond_esc(m.group(2))), text)
        text = re.sub(r'([\d\w])\^(\-?[^\s\-]+)[-\s]',
                      lambda m: '%s<sup>%s</sup> ' % (cond_esc(m.group(1)), cond_esc(m.group(2))), text)
        #text = re.sub(r'\$([^\$]+)\$', lambda m: '<i>%s</i>' % cond_esc(m.group(1)), text)
        text = re.sub(r'\$([^\$]+)\$', lambda m: '%s' % cond_esc(m.group(1)), text)
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
        text = format_math(text)
        text = format_style(text)
        return text

