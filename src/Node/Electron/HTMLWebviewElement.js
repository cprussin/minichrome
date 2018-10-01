"use strict";

exports.canGoBack = function (elem) {
  return function () {
    return elem.canGoBack();
  };
};

exports.goBack = function (elem) {
  return function () {
    elem.goBack();
  };
};

exports.canGoForward = function (elem) {
  return function () {
    return elem.canGoForward();
  };
};

exports.goForward = function (elem) {
  return function () {
    elem.goForward();
  };
};

exports.isDevToolsOpened = function (elem) {
  return function () {
    return elem.isDevToolsOpened();
  };
};

exports.openDevTools = function (elem) {
  return function () {
    elem.openDevTools();
  };
};

exports.getURL = function (elem) {
  return function () {
    return elem.getURL();
  };
};

exports.send = function (elem) {
  return function (cmd) {
    return function (args) {
      return function () {
        elem.send.apply(elem, [cmd].concat(args));
      };
    };
  };
};

exports.setZoomFactor = function (elem) {
  return function (zoomFactor) {
    return function () {
      elem.setZoomFactor(zoomFactor);
    };
  };
};

exports.findInPage = function (elem) {
  return function (term) {
    return function (forward) {
      return function (findNext) {
        return function () {
          elem.findInPage(term, { forward: forward, findNext: findNext });
        };
      };
    };
  };
};

exports.stopFindInPageImpl = function (elem) {
  return function (action) {
    return function () {
      elem.stopFindInPage(action);
    };
  };
};
