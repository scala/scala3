$(document).ready(function() {
    window.sr = ScrollReveal();
    sr.reveal(".scala-logo-container",  { duration: 2000, delay: 100, mobile: false });

    var hostname = new RegExp(location.host);
    $('a').each(function(){
        var url = $(this).attr("href");
        if (hostname.test(url) || url.slice(0, 1) == "#")
            $(this).addClass('local');
    });

    $("a").on('click', function(event) {
        if ($(this).hasClass("local") && this.hash != "") {
            event.preventDefault();
            var hash = this.hash;
            $('html, body').animate({ scrollTop: $(hash).offset().top }, 800, function() {
                window.location.hash = hash;
            });
        }
    });
});
