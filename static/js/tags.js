function setup_tag(taglist, tag) {
        var refid = taglist.readAttribute('data-refid');
        var name = tag.readAttribute('data-name');

        var a = new Element('a', {'href': '/refs/search?q=tag:"' + name + '"'});
        a.update(name);
        tag.appendChild(a);

        var e = new Element('span', {'class': "remove-tag"});
        e.update('X')
        tag.appendChild(e);
        tag.observe('mouseover', function() { e.style.display = 'block'; });
        tag.observe('mouseout', function() { e.style.display = 'none'; });
        e.observe('click', function() {
                        console.log('rm_tag ' + name);
                        new Ajax.Request('/refs/' + refid + '/tags/remove', {
                                method: 'POST',
                                requestHeaders: ajax_headers,
                                onSuccess: function() {
                                        console.log('Removed');
                                        taglist.removeChild(tag);
                                },
                                parameters: {'name': name}
                        });
        });
}

function setup_taglist(taglist) {
        var refid = taglist.readAttribute('data-refid')
        Element.select(taglist, 'li').each(function(tag) { setup_tag(taglist, tag); });

        var li = new Element('li', {'class': "add-tag"});
        taglist.appendChild(li);

        var form = new Element('form', {
                id: 'add-tag-form',
                action: '/refs/' + refid + '/tags/add',
                method: 'POST',
        });
        li.appendChild(form)

        var input = new Element('input', {
                name: 'name',
                type: 'text',
                autocomplete: 'off'
        });
        form.appendChild(input);

        var submit = new Element('input', {
                name: 'add-tag-submit',
                type: 'submit',
                value: '+'
        });
        form.appendChild(submit);

        var sug = new Element('ul', {
                id: 'suggestions',
                class: 'autocomplete',
        });

        input.observe('input', function(event) {
                new Ajax.Updater('suggestions', '/refs/tag_suggestions', {
                        parameters: { q: input.value },
                        requestHeaders: ajax_headers
                });
        });

        form.observe('submit', function(event) {
                Event.stop(event);
                form.request({
                        requestHeaders: ajax_headers,
                        onFailure: function(transport) { console.log('Error adding tag'); },
                        onSuccess: function(transport) {
                                input.value = '';
                                var name = transport.responseText;
                                var newtag = new Element('li', {'class': 'tag', 'data-name': name});
                                setup_tag(taglist, newtag);
                                li.insert({before: newtag});
                        }
                });
        });
}

document.observe('dom:loaded', function() {
        $$('ul.tag-list').each(setup_taglist);
});

