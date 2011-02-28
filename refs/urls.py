from django.conf.urls.defaults import *

urlpatterns = patterns('refs.views',
                       (r'^$', 'index'),
                       (r'^search$', 'search'),
                       (r'^(?P<ref_id>.+)/show$', 'show'),
                       (r'^(?P<ref_id>.+)/tags/add$', 'add_tag'),
                       (r'^(?P<ref_id>.+)/tags/remove$', 'rm_tag'),
)

