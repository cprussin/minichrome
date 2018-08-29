"use strict";

var app = require('electron').app;

exports.onReady = function (cb) {
  return function () {
    app.on('ready', cb);
  };
};

exports.onWindowAllClosed = function (cb) {
  return function () {
    app.on('window-all-closed', cb);
  };
};

exports.onActivate = function (cb) {
  return function () {
    app.on('activate', cb);
  };
};

exports.quit = function () {
  app.quit();
};
