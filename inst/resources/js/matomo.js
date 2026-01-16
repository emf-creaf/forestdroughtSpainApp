var _paq = window._paq = window._paq || [];
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function() {
  var u='https://stats-emf.creaf.cat/';
  _paq.push(['setTrackerUrl', u+'matomo.php']);
  _paq.push(['setSiteId', '6']);
  var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
  g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
})();

// Event Tracking Code
$(document).on('shiny:inputchanged', function(event) {
  if (/^map_out*/.test(event.name)) {
    _paq.push(['trackEvent', 'mapInputs', event.name, event.value, 1, {dimension1: event.value}]);
  }
  if (/^ts_out*/.test(event.name)) {
    _paq.push(['trackEvent', 'tsInputs', event.name, event.value, 2, {dimension1: event.value}]);
  }
});