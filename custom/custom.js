$(document).ready(function(){
    $('#header').prepend('<div class="popup" style="display:none;"></div>');
    $('.node').on('mouseenter', function(evt){
        $('.popup').html('<img src="' + $('title', this).html() + '.svg">');
        $('.popup').css({left: evt.pageX+30, top: evt.pageY-15}).show();
        $(this).on('mouseleave', function(){
            $('.popup').hide();
        });
    });
});
