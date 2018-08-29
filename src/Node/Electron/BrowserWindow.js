"use strict";

exports.createBrowserWindow = function (options) {
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

exports.loadURL = function (window) {
  return function (url) {
    return function () {
      window.loadURL(url);
    };
  };
};
