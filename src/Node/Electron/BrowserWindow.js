"use strict";

exports.createBrowserWindowImpl = function (options) {
  return function () {
    var BrowserWindow = require('electron').BrowserWindow;
    return new BrowserWindow(options);
  };
};

exports.setMenuImpl = function (window) {
  return function (menu) {
    return function () {
      window.setMenu(menu);
    };
  };
};

exports.clearMenu = function (window) {
  return function () {
    window.setMenu(null);
  };
};

exports.loadURLImpl = function (window) {
  return function (url) {
    return function (options) {
      return function () {
        window.loadURL(url, options);
      };
    };
  };
};

exports.loadFile = function (window) {
  return function (file) {
    return function () {
      window.loadFile(file);
    };
  };
};

exports.onceReadyToShow = function (window) {
  return function (cb) {
    return function () {
      window.once('ready-to-show', cb);
    };
  };
};

exports.show = function (window) {
  return function () {
    window.show();
  };
};

exports.onShow = function (window) {
  return function (cb) {
    return function () {
      window.on('show', cb);
    };
  };
};

exports.focusedWindowImpl = function (Just) {
  return function (Nothing) {
    return function () {
      var win = require('electron').BrowserWindow.getFocusedWindow();
      if (win) {
        return Just(win);
      } else {
        return Nothing;
      }
    };
  };
};
