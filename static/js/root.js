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
    JSONEditor.defaults.options.show_errors           = 'change';

    var el = $('#editor');
    var editor = new JSONEditor(el.get(0), {schema: {'$ref': '/schema/' + el.data('name')}});
    var navbar = $('nav.navbar');

    editor.on('change', function() {
        var errors = editor.validate();
        if (errors.length) {
            navbar.removeClass('valid invalid').addClass('invalid');
        }
        else {
            navbar.removeClass('valid invalid').addClass('valid');
        }
    });

    $('#errors', navbar).on('click', function () {
        var errors = editor.validate();
        alert(JSON.stringify(errors, null, 2));
    });

});

