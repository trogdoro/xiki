$(function(){
  xiki_txt = Xiki.unindent($('div[data-xiki=menu]').html());
  // Render jquery mobile interface

  $('body').html(Xiki.render_mobile_standalone());

  // Grab children from txt and add menu for each
  var children = Xiki.children(xiki_txt, "");
  var html = Xiki.children_to_html(children);
  //   console.log("html:", html)
  console.log(".ui-content:", $(".ui-content").length)

  $("div[data-role=content]").html(html);
  //   $(".ui-content").trigger("create");

  //   $(".ui-page").trigger("create");

  $(document).bind( "pagebeforechange", function(e, data) {

    if(typeof data.toPage != "string") return;

    var hash_orig = data.toPage.replace(/.+?(#|$)/, '');
    var hash = hash_orig.replace(/,/g, '/');

    Xiki.navigate(e, data, Xiki.children(xiki_txt, hash));
  });

    // Old






  //   $("body").trigger("create");

  //   $('div:eq(0)').html("abc");
  //   $('body').html("abc");

  //   $(".ui-content").blink();


  //   $('body').html(Xiki.render_mobile_standalone());
  //   $("div:first").trigger("create");
  //   $("div:first").page();
  //   $("div:first").blink();

  //   $(".ui-page").page();
  //   $(".ui-page").trigger("refresh");
  //   $("div[data-role=header]").trigger("create");
  //   $(".ui-page").trigger("refresh");

  // TODO: pass to xiki.children()
  //   $('div[data-xiki=menu]').blink();
})
