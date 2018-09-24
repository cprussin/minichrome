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
