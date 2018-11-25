// shuffle fn
(function(d){d.fn.shuffle=function(c){c=[];return this.each(function(){c.push(d(this).clone(true))}).each(function(a,b){d(b).replaceWith(c[a=Math.floor(Math.random()*c.length)]);c.splice(a,1)})};d.shuffle=function(a){return d(a).shuffle()}})(jQuery);

$(window).ready(function() {
  $.sort_by = 'sort_title';
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

      if (Number.isNaN(x_score)) return 1;
      if (Number.isNaN(y_score)) return -1;

      if ($.asc) {
        return x_score - y_score;
      } else {
        return y_score - x_score;
      }
    }).appendTo('#movies');
  });

  $(document).on('change', 'input[name=is_selected]:checked', function() {
    filter_movies();
  });

  $(document).on('keyup change', '#title_filter, #genre_filter', function() {
    filter_movies();
  })

  $(document).on('click', '#movies .card-container > span.badge', function() {
    $(this).parents('.card-container').toggleClass('selected');
    filter_movies();
  });

  $(document).on('click', 'button[data-dismiss="popover"]', function() {
    $(this).parents('.popover').popover('hide');
  });
});

function escapeRegExp(str) {
  return str.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}

filter_movies = function() {
  $('#movies .card-container').addClass('shown');

  $.each(['genre', 'title'], function(i, field) {
    var search_text = $('#' + field + '_filter').val();
    if (search_text === '') return;

    var search_regex = new RegExp("\\b" + escapeRegExp(search_text), 'i');

    $('#movies .card-container.shown').each(function(i, movie) {
      if ($(movie).data(field).toString().match(search_regex)) {
        $(movie).addClass('shown');
      } else {
        $(movie).removeClass('shown');
      }
    });
  });

  var is_selected = $('input[name=is_selected]:checked').val();
  
  if (is_selected === 'yes') {
    $('#movies .card-container.shown:not(.selected)').removeClass('shown');
  } else if (is_selected === 'no') {
    $('#movies .card-container.shown.selected').removeClass('shown');
  }

  $('#movies .card-container:not(.shown)').hide();
  $('#movies .card-container.shown').show();
}

function init_popovers() {
  $('#movies a[data-toggle=popover]').popover({
    template: '<div class="popover shadow-sm" role="tooltip"><div class="arrow"></div><div class="popover-body"></div></div>',
    html: true,
    trigger: 'click'
  });
}
