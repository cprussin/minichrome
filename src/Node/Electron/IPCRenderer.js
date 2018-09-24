"use strict";

exports.on = function (event) {
  return function (cb) {
    return function () {
      require('electron').ipcRenderer.on(event, function () {
        var args = Array.prototype.slice.call(arguments);
        cb(args.shift())(args)();
      });
    };
  };
};

exports.sendToHost = function (message) {
  return function (args) {
    return function () {
      var ipcRenderer = require('electron').ipcRenderer;
      ipcRenderer.sendToHost.apply(ipcRenderer, [message].concat(args));
    };
  };
};
