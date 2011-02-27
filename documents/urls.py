from django.conf.urls.defaults import *

urlpatterns = patterns('documents.views',
                       (r'^(?P<doc_id>.+)/$', 'fetch'),
                       (r'^(?P<doc_id>.+)/page/(?P<page_n>\d+)/(?P<format>png|svg)$', 'render_page'),
)
