// Show a loading circle while the server is busy (geocoding an address,
// applying URL parameters, rendering results) and hide it once Shiny is idle.
// Address lookups cover the inputs with #inputs_loading, where the person is
// looking; everything else uses the #loading circle below the inputs.
$(function() {
  var loader = $("#loading");
  var showTimer = null;
  var hideTimer = null;

  function showLoader() {
    clearTimeout(hideTimer);
    $("body").addClass("app-loading");
    loader.show();
  }

  function showGeocoding() {
    clearTimeout(hideTimer);
    clearTimeout(showTimer);
    showTimer = null;
    loader.hide();
    $("body").addClass("geocoding");
  }

  // Pages opened with an address geocode it right away; keep results hidden
  // until they're ready. Pages opened with a district just load the results.
  var params = new URLSearchParams(window.location.search);
  if (params.has("address") && !params.has("district")) {
    $("body").addClass("app-loading");
    showGeocoding();
  } else if (params.has("district")) {
    showLoader();
  }

  // Submitting hides the results right away, before the server shows the
  // results box, so its empty structure never flashes before the circle
  $(document).on("click", "#submit", function() {
    clearTimeout(showTimer);
    showTimer = null;
    showLoader();
  });

  // Searching for an address (Enter or the search button)
  $(document).on("shiny:inputchanged", function(event) {
    if (event.name === "address_input_search") {
      showGeocoding();
    }
  });

  $(document).on("shiny:busy", function() {
    clearTimeout(hideTimer);
    // Short delay so quick updates (e.g. changing party) don't flash the circle
    if (!showTimer && !loader.is(":visible") && !$("body").hasClass("geocoding")) {
      showTimer = setTimeout(showLoader, 300);
    }
  });

  $(document).on("shiny:idle", function() {
    clearTimeout(showTimer);
    showTimer = null;
    // Brief grace period bridges the gap between server round trips
    hideTimer = setTimeout(function() {
      loader.hide();
      $("body").removeClass("app-loading geocoding");
    }, 250);
  });
});
