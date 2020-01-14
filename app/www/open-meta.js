Shiny.addCustomMessageHandler(
         'redirect', function(url) {
   window.location = url;
});

Shiny.addCustomMessageHandler(
         'setCookie', function(pList) {
   if(pList.days>0) {
      Cookies.set(pList.cookieType, escape(pList.cookie), 
         { expires: pList.days });
   } else {
      Cookies.set(pList.cookieType, escape(pList.cookie));
   }
});

Shiny.addCustomMessageHandler(
         'getCookie', function(pList) {
   var cookie = Cookies.get(pList.cookieType);
   if (typeof cookie == "undefined") { cookie = ""; }
   Shiny.onInputChange("js.".concat(pList.cookieType), cookie);
});

Shiny.addCustomMessageHandler(
         'removeCookie', function(pList) {
   Cookies.remove(pList.cookieType);
});