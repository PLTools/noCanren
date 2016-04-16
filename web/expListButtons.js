      //Create the button funtionality
      $(document).ready(function() {
        $('#expandList')
          .unbind('click')
          .click( function() {
            $('.collapsed').addClass('expanded');
            $('.collapsed').children().show('medium');
        });
        $('#collapseList')
          .unbind('click')
          .click( function() {
            $('.collapsed').removeClass('expanded');
            $('.collapsed').children().hide('medium');
        });
      });
