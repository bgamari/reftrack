from django.conf.urls.defaults import *
from django.conf import settings

urlpatterns = patterns('',
                       (r'^refs/', include('refs.urls')),
                       (r'^documents/', include('documents.urls')),
)

if settings.DEBUG:
        urlpatterns += patterns('', 
                                (r'^' + settings.MEDIA_URL.lstrip('/') + r'(.*)$', 
                                 'django.views.static.serve',
                                 {'document_root': settings.MEDIA_ROOT}))

