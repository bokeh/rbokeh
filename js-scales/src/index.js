import { scaleLinear, scalePow, scaleSqrt, scaleLog, scaleQuantize,
  scaleThreshold, scaleOrdinal, scaleImplicit } from 'd3-scale';

const funcs = {
  // continuous to continuous
  scaleLinear,
  scalePow,
  scaleSqrt,
  scaleLog,
  // continuous to discrete
  scaleQuantize,
  scaleThreshold,
  // discrete to discrete,
  scaleOrdinal,
  scaleImplicit,
  vectorize: function(f, xs) {
    var new_xs = new Array(xs.length);
    for(var i = 0; i < xs.length; i++) {
      new_xs[i] = f(xs[i]);
    }
    return new_xs;
  }
}

window.RBK = funcs;
