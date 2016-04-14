var make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Menu = elm.Native.Menu || {};

  var Utils = Elm.Native.Utils.make(elm);

  var fromArray = Utils.list;

  var decode = function(e) {

    var button     = e.target;                // element_
    var container  = button.nextSibling;      // container_
    var menu       = container.childNodes[1]; // forElement_

    var offsetTops = [];
    var items      = menu.childNodes;
    for (var i = 0; i < items.length; i++) {
      offsetTops.push(items[i].offsetTop);
    }

    var offsetHeights = [];
    var items      = menu.childNodes;
    for (var i = 0; i < items.length; i++) {
      offsetHeights.push(items[i].offsetHeight);
    }

    var result = {
      button: {
        offsetTop: button.offsetTop,
        offsetLeft: button.offsetLeft,
        offsetHeight: button.offsetHeight,
        bounds: button.getBoundingClientRect()
      },
      menu: {
        offsetTop: menu.offsetTop,
        offsetLeft: menu.offsetLeft,
        offsetHeight: menu.offsetHeight,
        bounds: menu.getBoundingClientRect()
      },
      container: {
        offsetTop: container.offsetTop,
        offsetLeft: container.offsetLeft,
        offsetHeight: container.offsetHeight,
        bounds: container.getBoundingClientRect()
      },
      offsetTops: fromArray(offsetTops),
      offsetHeights: fromArray(offsetHeights)
    };
    return result;

  };


  if (elm.Native.Menu.values) return elm.Native.Menu.values;

  return elm.Native.Menu.values = {
    'decode': decode
  };
};

Elm.Native.Menu = {};
Elm.Native.Menu.make = make;
