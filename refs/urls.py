from django.conf.urls.defaults import *

urlpatterns = patterns('refs.views',
                       (r'^$', 'index'),
                       (r'^search$', 'search'),
                       (r'^search_results$', 'search_results'),
                       (r'^tags$', 'tag_list'),
                       (r'^(?P<ref_id>.+)/show$', 'show'),
                       (r'^(?P<ref_id>.+)/tags/add$', 'add_tag'),
                       (r'^(?P<ref_id>.+)/tags/remove$', 'rm_tag'),
                       (r'^bulk_tag/add$', 'bulk_add_tag'),
                       (r'^bulk_tag/remove$', 'bulk_remove_tag'),
)

