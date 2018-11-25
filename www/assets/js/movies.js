// shuffle fn
(function(d){d.fn.shuffle=function(c){c=[];return this.each(function(){c.push(d(this).clone(true))}).each(function(a,b){d(b).replaceWith(c[a=Math.floor(Math.random()*c.length)]);c.splice(a,1)})};d.shuffle=function(a){return d(a).shuffle()}})(jQuery);

$(window).ready(function() {
  $.sort_by = 'title';
  $.asc = true;

  $(document).on('click', '#sort_by label', function() {
    var sort_by = $(this).children('input').val();

    $.asc = ($.sort_by === sort_by ? !$.asc : true);
    $.sort_by = sort_by;

    if ($.sort_by === 'random') {
      $('#movies .card-container').shuffle();
      return;
    }

    $('#movies .card-container').sort(function(x, y) {
      var x_score = parseInt($(x).data($.sort_by));
      var y_score = parseInt($(y).data($.sort_by));

      if ($.asc) {
        return x_score - y_score;
      } else {
        return y_score - x_score;
      }
    }).appendTo('#movies');
  });

  $(document).on('click', '#is_selected label', function() {
    var val = $(this).children('input').val();

    if (val === 'yes') {
      $('#movies .card-container > span.badge:not(.selected)').parents('.card-container').hide();
      $('#movies .card-container > span.badge.selected').parents('.card-container').show();
    } else if (val === 'no') {
      $('#movies .card-container > span.badge.selected').parents('.card-container').hide();
      $('#movies .card-container > span.badge:not(.selected)').parents('.card-container').show();
    } else {
      $('#movies .card-container').show();
    }
  });

  $(document).on('click', '#movies .card-container > span.badge', function() {
    $(this).toggleClass('selected');
  });

  $(document).on('click', 'button[data-dismiss="popover"]', function() {
    $(this).parents('.popover').popover('hide');
  });
});

function init_popovers() {
  $('#movies a[data-toggle=popover]').popover({
    template: '<div class="popover shadow-sm" role="tooltip"><div class="arrow"></div><div class="popover-body"></div></div>',
    html: true,
    trigger: 'click'
  });
}
