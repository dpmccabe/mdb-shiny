$(window).ready(function() {
  $(document).on('click', '#sort_by label', function() {
    Shiny.onInputChange(
    	'sort_by_trigger',
			{
        val: $(this).children('input').val(),
        nonce: Math.random()
      }
    );
  });

  $(document).on('click', '#is_selected label', function() {
    Shiny.onInputChange(
    	'is_selected_trigger',
			{
        val: $(this).children('input').val(),
        nonce: Math.random()
      }
    );
  });

  $(document).on('click', 'button[data-dismiss="popover"]', function() {
    $(this).parents('.popover').popover('hide');
  });

  $(document).on('click', '#movies .card-container > span.badge', function() {
    Shiny.onInputChange(
    	'toggle_selected',
			{
        val: $(this).data('id'),
        nonce: Math.random()
      }
    );
  });
});

function init_popovers() {
  $('#movies a[data-toggle=popover]').popover({
    template: '<div class="popover shadow-sm" role="tooltip"><div class="arrow"></div><div class="popover-body"></div></div>',
    html: true,
    trigger: 'click'
  });
}
