// Offline browser-event checks: node tests/check-diagnostics.cjs
const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const handlers = {};
const reports = [];
const document = { addEventListener: (name, handler) => { handlers[name] = handler; } };
const window = {
  location: { href: "https://sws.qa.fao.org/link/FisheriesAggregation/" },
  jQuery: () => ({ on: (name, handler) => { handlers[name] = handler; } }),
  Shiny: { setInputValue: (name, event) => { reports.push({ name, event }); } }
};
vm.runInNewContext(fs.readFileSync("www/fisheries-diagnostics.js", "utf8"), {
  window, document, URL, console: { error() {} }
});
const settings = { sTableId: "DataTables_Table_3", ajax: { url: "session/abc/dataobj/raw?w=secret" } };
const xhr = { status: 502, getResponseHeader: () => "text/html" };
handlers["xhr.dt"]({}, settings, { data: [] }, xhr);
assert.equal(reports.length, 0);
handlers["xhr.dt"]({}, settings, null, xhr);
assert.equal(reports[0].event.status, 502);
assert.equal(reports[0].event.path, "/link/FisheriesAggregation/session/abc/dataobj/raw");
assert.equal(reports[0].event.table, "DataTables_Table_3");
handlers.securitypolicyviolation({
  disposition: "enforce", effectiveDirective: "connect-src",
  blockedURI: "https://external.example/script?token=secret", statusCode: 200
});
assert.equal(reports[1].event.directive, "connect-src");
assert.equal(reports[1].event.path, "/script");
handlers.securitypolicyviolation({ disposition: "report" });
assert.equal(reports.length, 2);
assert.ok(!JSON.stringify(reports).includes("secret"));
console.log("Browser diagnostics checks passed");
