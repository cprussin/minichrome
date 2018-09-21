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
