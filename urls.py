from django.conf.urls.defaults import *

urlpatterns = patterns('',
                       (r'^refs/', include('refs.urls')),
                       (r'^documents/', include('documents.urls')),
)
