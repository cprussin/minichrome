"use strict";

exports.onNewWindow = function (webContents) {
  return function (cb) {
    return function () {
      webContents.on('new-window', function (event, url) {
        cb(event)(url)();
      });
    };
  };
};

exports.beforeInputEvent = function(webContents) {
  return function (cb) {
    return function () {
      webContents.on('before-input-event', function (event, input) {
        cb(event)(input)();
      });
    };
  };
};

exports.canGoBack = function (webContents) {
  return webContents.canGoBack();
};

exports.goBack = function (webContents) {
  return function () {
    return webContents.goBack();
  }
};

exports.canGoForward = function (webContents) {
  return webContents.canGoForward();
};

exports.goForward = function (webContents) {
  return function () {
    return webContents.goForward();
  }
};
