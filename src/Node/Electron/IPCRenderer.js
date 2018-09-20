"use strict";

exports.on = function (event) {
  return function (cb) {
    return function () {
      require('electron').ipcRenderer.on(event, function (evt, args) {
        cb(evt)(args)();
      });
    };
  };
};
