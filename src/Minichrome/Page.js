"use strict";

exports.addPassiveEventListener = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          return target.addEventListener(type, listener, {
            passive: true,
            capture: useCapture
          });
        };
      };
    };
  };
};

exports.addOnceEventListener = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          return target.addEventListener(type, listener, {
            once: true,
            capture: useCapture
          });
        };
      };
    };
  };
};
