  // Text input
  var returnTextInputBinding = new Shiny.InputBinding();
  $.extend(returnTextInputBinding, {
    find: function(scope) {
      return $(scope).find('input[type="text"]');
    },
    getId: function(el) {
      return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
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
  Shiny.inputBindings.register(returnTextInputBinding, 'returnTextInput');


// $.extend(myTextInputBinding, {
//   find: function(scope) {
//     return $(scope).find('.myTextInput');
//   },
//   getId: function(el) {
//     // return InputBinding.prototype.getId.call(this, el) || el.name;
//     return $(el).attr('id')
//   },
//   getValue: function(el) {
//     return el.value;
//   },
//   setValue: function(el, value) {
//     el.value = value;
//   },
//   subscribe: function(el, callback) {
//     $(el).on('keyup.textInputBinding input.textInputBinding', function(event) {
//       if(event.keyCode == 13) { //if enter
//         callback()
//       }
//     });
//     // $(el).on('focusout.myTextInputBinding', function(event) { // on losing focus
//     //   callback();
//     // });
//   },
//   unsubscribe: function(el) {
//     $(el).off('.myTextInputBinding');
//   },
//   receiveMessage: function(el, data) {
//     if (data.hasOwnProperty('value'))
//       this.setValue(el, data.value);
//     if (data.hasOwnProperty('label'))
//       $(el).parent().find('label[for=' + el.id + ']').text(data.label);
//     $(el).trigger('change');
//   },
//   getState: function(el) {
//     return {
//       label: $(el).parent().find('label[for=' + el.id + ']').text(),
//       value: el.value
//     };
//   }
//   // },
//   // getRatePolicy: function() {
//   //   return {
//   //     policy: 'debounce',
//   //     delay: 250
//   //   };
//   // }
// });