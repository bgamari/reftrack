from django.conf.urls.defaults import *

urlpatterns = patterns('refs.views',
                       (r'^$', 'index'),
                       (r'^search$', 'search'),
                       (r'^(?P<ref_id>[_\.:\w\d]+)/$', 'show'),
                       (r'^^(?P<ref_id>[_\.:\w\d]+)/page/(?P<page_n>\d+)/(?P<format>png|svg)$', 'render_page'),
)
