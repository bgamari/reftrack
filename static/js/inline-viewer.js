Element.prototype.registerHotkey = function(key, f) {
        var keycode = null;
        if (key.keycode) keycode = key.keycode;
        if (key.letter) keycode = key.letter.toUpperCase().charCodeAt(0);
        if (keycode == null) console.log('uh oh');
        document.observe('keydown', function(event) {
                if (event.keyCode == keycode) f();
        });
}

function DocViewer(historyState) {
        this.doc_id = null;
        this.format = 'png';
        this.width = 800;
        this.page_n = 0;
        this.n_pages = 0;

        this.historyState = historyState;
        if (historyState) {
                window.onpopstate = function(event) {
                        this.page_n = event.state['page_n'];
                        this.update_page();
                };
        }

        var dv = this;
        $('zoom-out').observe('click', function(){ dv.zoomOut(); });
        $('zoom-in').observe('click', function(){ dv.zoomIn(); });
        $('prev-page').observe('click', function(){ dv.prevPage(); });
        $('next-page').observe('click', function(){ dv.nextPage(); });
        $('page-num').observe('change', function(){ Number(dv.setPage($('page-num').value)); });

        $('doc-viewer').registerHotkey({keycode: 189}, function(){ dv.zoomOut(); }); // key: -
        $('doc-viewer').registerHotkey({keycode: 187}, function(){ dv.zoomIn(); }); // key: +
        $('doc-viewer').registerHotkey({letter: 'n'}, function(){ dv.nextPage(); });
        $('doc-viewer').registerHotkey({letter: 'j'}, function(){ dv.nextPage(); });
        $('doc-viewer').registerHotkey({letter: 'p'}, function(){ dv.prevPage(); });
        $('doc-viewer').registerHotkey({letter: 'k'}, function(){ dv.prevPage(); });
}

DocViewer.prototype.setDocument = function(doc_id) {
        dv = this;
        new Ajax.Request('/documents/' + doc_id + '/info', {
                method: 'GET',
                onSuccess: function(response) {
                        var doc = JSON.parse(response.responseText);
                        dv.n_pages = doc.n_pages;
                        $('page-num').max = dv.n_pages;
                        $('num-pages').textContent = dv.n_pages;
                        dv.updateButtons();
                },
                onFailure: function(response) {
                        console.log('get document info failed');
                }
        });
        this.doc_id = doc_id;
}

DocViewer.prototype.updatePage = function() {
        $('doc-viewer').addClassName('loading');
        $('page').onload = function() {
                $('doc-viewer').removeClassName('loading');
        };
        $('page').src = "/documents/" + this.doc_id + "/page/" + this.page_n + "?format=" + this.format + "&width=" + this.width;
        $('page').alt = "Page " + this.page_n;
        $('page-num').value = this.page_n+1;

        if (this.historyState) {
                var url = "/documents/" + this.doc_id + "/view/" + this.page_n + "?format=" + this.format + "&width=" + this.width;
                history.pushState({page_n: this.page_n}, 'page ' + this.page_n, url);
        }
        this.updateButtons();
}

DocViewer.prototype.updateButtons = function() {
        if (this.page_n == this.n_pages-1)
                $('next-page').style.visibility = 'hidden';
        else
                $('next-page').style.visibility = 'visible';

        if (this.page_n == 0)
                $('prev-page').style.visibility = 'hidden';
        else
                $('prev-page').style.visibility = 'visible';
};

DocViewer.prototype.nextPage = function() {
        this.page_n += 1;
        this.updatePage();
};

DocViewer.prototype.prevPage = function() {
        this.page_n -= 1;
        this.updatePage();
};

DocViewer.prototype.setPage = function(n) {
        this.page_n = n;
        this.updatePage();
};

DocViewer.prototype.zoomIn = function() {
        this.width = Math.round(this.width*1.2);
        this.updatePage();
};

DocViewer.prototype.zoomOut = function() {
        this.width = Math.round(this.width/1.2);
        this.updatePage();
};

