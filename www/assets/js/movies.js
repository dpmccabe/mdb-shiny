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

  $(document).on('click', 'button[data-dismiss="popover"]', function() {
  	console.log('hide');
    $(this).parents('.popover').popover('hide');
  });

  $(document).on('mousedown', '.popover', function(e) {
    e.preventDefault();
  });
});

function init_popovers() {
  $('#movies a[data-toggle=popover]').popover({
    template: '<div class="popover shadow-sm" role="tooltip"><div class="arrow"></div><div class="popover-body"></div></div>',
    html: true,
    trigger: 'focus'
  });
}
