function registerHoverEvent () {
    $('.node:has(a)').on('mouseenter', function (evt) {
        $('.popup').html('<img src="' + $('a', this).attr('xlink:href') + '">');
        $('.popup').css({ left: evt.pageX + 30, top: evt.pageY - 15 }).show();
        $(this).on('mouseleave', function () {
            $('.popup').hide();
        });
    });
}

$(document).ready(function () {
    $('#header').prepend('<div class="popup" style="display:none;"></div>');
    registerHoverEvent();

    var target = document.getElementById('notebook');
    var observer = new MutationObserver(function (mutations) {
        mutations.forEach(function (mutation) {
            addedNodes = $(mutation.addedNodes);
            if (addedNodes != null) {
                addedNodes.each(registerHoverEvent);
            }
        });
    });

    var config = {
        childList: true,
        subtree: true
    };

    observer.observe(target, config);
});
