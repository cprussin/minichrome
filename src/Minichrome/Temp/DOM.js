"use strict";

exports.matches = function (selector) {
  return function (elem) {
    return function () {
      return elem.matches(selector);
    };
  };
};

exports.scrollIntoView = function (elem) {
  return function () {
    elem.scrollIntoView();
  };
};

exports.setWindowTitle = function (document) {
  return function (title) {
    return function () {
      document.title = title;
    };
  };
};
