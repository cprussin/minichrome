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
    webContents.goBack();
  };
};

exports.canGoForward = function (webContents) {
  return webContents.canGoForward();
};

exports.goForward = function (webContents) {
  return function () {
    webContents.goForward();
  };
};

exports.openDevTools = function (webContents) {
  return function () {
    webContents.openDevTools({ mode: 'detach' });
  };
};

exports.setDevToolsWebContents = function (webContents) {
  return function (target) {
    return function () {
      webContents.setDevToolsWebContents(target);
    };
  };
};

exports.focusedWebContentsImpl = function (Just) {
  return function (Nothing) {
    return function () {
      var contents = require('electron').webContents.getFocusedWebContents();
      if (contents) {
        return Just(contents);
      } else {
        return Nothing;
      }
    };
  };
};

exports.send = function (webContents) {
  return function (cmd) {
    return function (args) {
      return function () {
        webContents.send.apply(webContents, [cmd].concat(args));
      };
    };
  };
};
