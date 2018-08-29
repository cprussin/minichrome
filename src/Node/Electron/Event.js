"use strict";

exports.preventDefault = function (event) {
  return function () {
    event.preventDefault();
  };
};
