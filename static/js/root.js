//
// Inject JSON Editor

$(document).ready(function () {

    JSONEditor.defaults.options.theme                 = 'bootstrap3';
    JSONEditor.defaults.options.iconlib               = 'bootstrap3';
    JSONEditor.defaults.options.ajax                  = true;
    JSONEditor.defaults.options.disable_array_reorder = true;
    JSONEditor.defaults.options.disable_edit_json     = true;
    JSONEditor.defaults.options.disable_properties    = true;
    JSONEditor.defaults.options.required_by_default   = true;
    JSONEditor.defaults.options.show_errors           = 'always';

    var el = $('#editor');
    var editor = new JSONEditor(el.get(0), {schema: {'$ref': '/schema/' + el.data('name')}});
    var navbar = $('nav.navbar');
    var _status = $('#status', navbar);
    var _rev = $('#rev', navbar);

    var populate = function (url) {
        $.getJSON(url, function (data) {
            editor.setValue(data);
        });
    };

    var commit = function (url) {
        var errors = editor.validate();
        if (errors.length) {
            return null;
        }
        var dataObject = editor.getValue();
        if (dataObject) {
            $.ajax({
                url: url,
                type: 'POST',
                data: JSON.stringify(dataObject),
                contentType: "application/json; charset=utf-8",
                dataType: 'json',
                success: function (revision) {
                    silence();
                    renderRevision(revision);
                },
                error: function (xhr, textStatus, thrown) {
                    emergeError(textStatus + ': ' + thrown);
                },
                complete: function () {
                    stopProgressAnimation();
                }
            });
        }
    };

    //
    // Knick-knacks

    var animateProgress = function () {
        navbar.animate({'background-position-x': "+=160"}, 3000, 'linear', function () { animateProgress(); });
    };

    var startProgressAnimation = function () {
        navbar.stop().addClass('pending');
        animateProgress();
    };

    var stopProgressAnimation = function () {
        navbar.stop().removeClass('pending');
    };

    var emergeError = function (status) {
        navbar.removeClass('valid invalid').addClass('invalid');
        _status.html(status);
    };

    var silence = function () {
        navbar.removeClass('valid invalid').addClass('valid');
        _status.html('');
    };

    var renderRevision = function (revision) {
        _rev.html('HEAD @ ' + revision.substr(0, 12));
    };

    editor.on('change', function() {
        var errors = editor.validate();
        if (errors.length) { emergeError('Non-conforming object'); } else { silence(); }
    });

    $('#commit', navbar).on('click', function () {
        startProgressAnimation();
        commit($(this).attr('href'));
        return false;
    });

    $('#refresh', navbar).on('click', function () {
        startProgressAnimation();
        $(this).children('form').submit();
        return false;
    })

    populate($('#commit', navbar).attr('href'));

    $.getJSON('/data/revision', renderRevision);

    //
    // Assets preloading

    var preloadImage = function (url) {
        var i = new Image();
        i.src = url;
        return i;
    };

    preloadImage('/img/pr.png');

});

