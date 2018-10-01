"use strict";

exports.setWindowTitle = function (document) {
  return function (title) {
    return function () {
      document.title = title;
    };
  };
};
