export const rawIndex = function(i) {
  return function(xs) {
    return xs[i];
  }
}

export const offsetWithImpl = function (offset) {
  return function (operator) {
    return function(xs) {
      let xsLen = xs.length;
      let ysLen = xsLen - offset;
      let ys = new Array(ysLen);
      for (let i = 0; i < ysLen; i++) {
        ys[i] = operator(xs[i])(xs[i + offset]);
      }
      return ys;
    }
  }
}
