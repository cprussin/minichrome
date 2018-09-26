"use strict";

var clipboard = require('electron').clipboard;

exports.writeText = function (text) {
  return function () {
    clipboard.writeText(text);
  };
};
