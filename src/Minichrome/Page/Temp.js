"use strict";

exports.addEventListenerImpl = function (type) {
  return function (listener) {
    return function (target) {
      return function (options) {
        return function () {
          return target.addEventListener(type, listener, options);
        };
      };
    };
  };
};

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
