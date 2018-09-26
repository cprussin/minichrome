"use strict";

exports.setTitle = function (document) {
  return function (title) {
    return function () {
      document.title = title;
    };
  };
};
