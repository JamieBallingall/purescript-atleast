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

export const foldr1Impl = function (f) {
  return function (xs) {
    let len = xs.length;
    let acc = xs[len - 1];
    for (let i = len - 2; i >= 0; i--) {
      acc = f(xs[i])(acc);
    }
    return acc;
  }
}

export const foldl1Impl = function (f) {
  return function (xs) {
    let acc = xs[0];
    let len = xs.length;
    for (let i = 1; i < len; i++) {
      acc = f(acc)(xs[i]);
    }
    return acc;
  }
}
