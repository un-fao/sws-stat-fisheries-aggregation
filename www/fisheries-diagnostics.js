(function () {
  "use strict";

  // Deliberately omit query strings, response bodies and authentication headers.
  function pathOnly(url) {
    try {
      return new URL(url, window.location.href).pathname;
    } catch (_) {
      return "";
    }
  }

  function report(event) {
    console.error("[Fisheries diagnostics]", event);
    if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
      window.Shiny.setInputValue("fisheries_diagnostic", event, { priority: "event" });
    }
  }

  // DT ships its libraries locally. Report the actual failing session request
  // so proxy/authentication errors can be distinguished from a CSP violation.
  if (window.jQuery) {
    window.jQuery(document).on("xhr.dt", function (_, settings, json, xhr) {
      if (json !== null || !xhr) return;
      var ajax = settings.ajax || {};
      report({
        kind: "datatable_ajax",
        table: settings.sTableId || "",
        path: pathOnly(typeof ajax === "string" ? ajax : ajax.url),
        status: xhr.status || 0,
        content_type: xhr.getResponseHeader("Content-Type") || ""
      });
    });
  }

  document.addEventListener("securitypolicyviolation", function (event) {
    if (event.disposition !== "enforce") return;
    report({
      kind: "csp",
      directive: event.effectiveDirective,
      path: pathOnly(event.blockedURI),
      status: event.statusCode || 0
    });
  });
})();
