/* CUSTOM JS
   ========================================================================== */

$('[data-toggle="offcanvas"]').click(function () {
    $('.aside-offcanvas').toggleClass('off');
    if($('#off-canvas-icon').hasClass('icon-left')){
        $('#off-canvas-icon').removeClass('icon-left').addClass('icon-right');
    } else {
        $('#off-canvas-icon').removeClass('icon-right').addClass('icon-left');
    }
});
$('[data-toggle="hidecanvas"]').click(function () {
    $('.labs-section-aside').toggleClass('off');
    if($('#hidecanvas-icon').hasClass('icon-left')){
        $('#hidecanvas-icon').removeClass('icon-left').addClass('icon-right');
    } else {
        $('#hidecanvas-icon').removeClass('icon-right').addClass('icon-left');
    }
});
$('[data-toggle="togglesearch"]').click(function () {
	$('#search-user').toggleClass('active');
});

