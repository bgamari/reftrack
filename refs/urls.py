from django.conf.urls.defaults import *

urlpatterns = patterns('refs.views',
                       (r'^$', 'index'),
                       (r'^search$', 'search'),
                       (r'^(?P<ref_id>.+)/$', 'show'),
)
