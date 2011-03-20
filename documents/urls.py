from django.conf.urls.defaults import *

urlpatterns = patterns('documents.views',
                       (r'^(?P<doc_id>.+)/$', 'fetch'),
                       (r'^(?P<doc_id>.+)/info$', 'info'),
                       (r'^(?P<doc_id>.+)/page/(?P<page_n>\d+)$', 'render_page'),
                       (r'^(?P<doc_id>.+)/view$', 'view'),
                       (r'^(?P<doc_id>.+)/view/(?P<page_n>\d+)$', 'view'),
)
