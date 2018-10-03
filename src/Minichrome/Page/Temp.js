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
