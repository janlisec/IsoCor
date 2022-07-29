var h = 0
$(document).on("shiny:connected", function(e) {
  h = window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight
  Shiny.setInputValue("CurrentScreenHeight", h, {priority: "event"});
});
$(window).resize(function(e) {
  h = window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight
  Shiny.setInputValue("CurrentScreenHeight", h, {priority: "event"});
});
