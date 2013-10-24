  // Text input

// var navbarBinding = new Shiny.InputBinding();

  var textInputBinding = new InputBinding();
  // var textInputBinding = new Shiny.InputBinding();
  $.extend(textInputBinding, {
    find: function(scope) {
      return $(scope).find('input[type="text"]');
    },
    getId: function(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function(el) {
      return el.value;
    },
    setValue: function(el, value) {
      el.value = value;
    },
    subscribe: function(el, callback) {
      // $(el).on('keyup.textInputBinding input.textInputBinding', function(event) {
      //   callback(true);
      // });
      $(el).on('change.textInputBinding', function(event) {
        callback(false);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.textInputBinding');
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for=' + el.id + ']').text(data.label);

      $(el).trigger('change');
    },
    getState: function(el) {
      return {
        label: $(el).parent().find('label[for=' + el.id + ']').text(),
        value: el.value
      };
    },
    getRatePolicy: function() {
      return {
        policy: 'debounce',
        delay: 250
      };
    }
  });
  // Shiny.inputBindings.register(textInputBinding, 'shiny.textInput');
  inputBindings.register(textInputBinding, 'shiny.textInput');
