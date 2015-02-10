"use strict";

%%[(8 javascript).debugStuff
var traceOn = false;

function trace(m, s) {
  if (traceOn) {
    console.log(m + ": " + s);
  }
}

var evalCounter = 0;
var nodeCounter = 0;
%%]

%%[(100 javascript) -8.debugStuff
%%]

%%[(8 javascript)
// interface to apply
function _a_(f,x) {
  return f.__aN__(x);
}
%%]

%%[(8 javascript)
// interface to eval
function _e_(x) {
  var x_, xx, x_next;
%%[[8
  trace("> _e_", x);
%%][100
%%]]
  if (x !== undefined && x !== null && x.__eOrV__ !== undefined) {
    x_ = x;
    do {
      if (typeof x.__eOrV__ === 'function' && !x.fe) {
%%[[8
        trace(">> _e_()", typeof x + "/" + typeof x.__eOrV__ + ":" + x);
%%][100
%%]]
        xx = x.__eOrV__();
        x.__eOrV__ = xx;
        x = xx;
%%[[8
        if (x !== undefined && x !== null && x.__eOrV__ !== undefined) {
          trace("<< _e_()", typeof x + "/" + typeof x.__eOrV__ + ":" + x);
        } else {
          trace("<< _e_()", typeof x + ":" + x);
        }
%%][100
%%]]
      } else {
%%[[8
        trace(">> _e_", typeof x + "/" + typeof x.__eOrV__ + ":" + x);
%%][100
%%]]
        x = x.__eOrV__;
%%[[8
        if (x !== undefined && x !== null && x.__eOrV__ !== undefined) {
          trace("<< _e_()", typeof x + "/" + typeof x.__eOrV__ + ":" + x);
        } else {
          trace("<< _e_()", typeof x + ":" + x);
        }
%%][100
%%]]
      }
    } while (x !== undefined && x !== null && x.__eOrV__ !== undefined);
    while (x_ !== undefined && x_ !== null && x_.__eOrV__ !== undefined) {
      x_next = x_.__eOrV__;
      x_.__eOrV__ = x;
      x_.fe = true;
      x_ = x_next;
    }
  }
%%[[8
  evalCounter += 1;
  trace("< _e_", x);
%%][100
%%]]
  return x;
}

function _A_undersat_(fun, args) {
  // this.needs = fun.needs - args.length;
  this.fun = fun;
  this.args = args;
%%[[8
  nodeCounter += 1;
  this.nodeId = nodeCounter;
%%][100
%%]]
}

// Apply node, not enough args
_A_undersat_.prototype = {
  __aN__ : function (args) {
    var needs, fun;
    needs = this.needsNrArgs();
    if (args.length < needs) {
      return new _A_undersat_(this, args);
    } else if (args.length === needs) {
%%[[8
      trace("> _A_undersat_.__aN__(=sat)", this + "(|args#" + args.length + "=(|" + args + "|)|)");
%%][100
%%]]
      return this.fun.__aN__(this.args.concat(args));
    } else {
%%[[8
      trace("> _A_undersat_.__aN__(>sat)", this + "(|args#" + args.length + "=(|" + args.slice(0, needs) + "|)+(|" + args.slice(needs) + "|)|)");
%%][100
%%]]
      fun = _e_(this.__aN__(args.slice(0, needs)));
      return {
        __eOrV__ : function () {
          return fun.__aN__(args.slice(needs));
        }
      };
    }
  },
  needsNrArgs : function () {
    return this.fun.needsNrArgs() - this.args.length;
  },
%%[[8
  getName : function () {
    return "A-" + this.needsNrArgs() + "#" + this.nodeId + "'";
  },
  toString : function () {
    return "(" + this.getName() + "=" + this.fun + "@" + this.args + ")";
  }
%%][100
%%]]
};

// Apply node, unknown how much is missing or too much
_A_.prototype = {
  __aN__ : function (args) {
    var fun = _e_(this);
    return {
      __eOrV__ : function () {
        return fun.__aN__(args);
      }
    };
  },
%%[[8
  getName : function () {
    return "A" + this.args.length + "#" + this.nodeId + "'" + this.fun.getName();
  },
  getVal : function () {
    return "V#" + this.nodeId + "'" + this.__eOrV__;
  },
  toString : function () {
    if (typeof this.__eOrV__ === 'function') {
      return "(" + this.getName() + "@args#" + this.args.length + "=(|" + this.args + "|))";
    } else {
      return "(" + this.getVal() + ")";
    }
  }
%%][100
%%]]
};
function _A_(fun, args) {
  this.__eOrV__ = function () {
%%[[8
    trace("> _A_.__eOrV__", fun + "(|args#" + args.length + "=" + args + "|)");
%%][100
%%]]
    var x = fun.__aN__(args);
%%[[8
    trace("< _A_.__eOrV__", fun + "(|args#" + args.length + "=" + args + "|)");
    trace("<   ->", this + " -> " + x);
%%][100
%%]]
    return x;
  };
  this.fe = false;
%%[[8
  this.fun = fun;
  this.args = args;
  nodeCounter += 1;
  this.nodeId = nodeCounter;
%%][100
%%]]
}

%%[[8
function _F_(name, evalN) {
%%][100
function _F_(evalN) {
%%]]
  //this.needs = evalN.length;
  this.__evN__ = evalN;
%%[[8
  this.name = name;
  nodeCounter += 1;
  this.nodeId = nodeCounter;
%%][100
%%]]
}

// Function node
_F_.prototype = {
  __aN__ : function (args) {
    var x, fun, remargs;
    if (args.length < this.__evN__.length) {
      return new _A_undersat_(this, args);
    } else if (args.length === this.__evN__.length) {
%%[[8
      trace("> _F_.__aN__(=sat)", this + "(|args#" + args.length + "=" + args + "|)");
%%][100
%%]]
      x = this.__evN__.apply(null, args);
%%[[8
      trace("< _F_.__aN__(=sat)", this + "(|args#" + args.length + "=" + args + "|)");
      trace("<   ->", x);
%%][100
%%]]
      return x;
    } else {
%%[[8
      trace("> _F_.__aN__(>sat)", this + "(|needs#" + this.__evN__.length + "args#" + args.length + "=" + args + "|)");
%%][100
%%]]
      fun = _e_(this.__evN__.apply(null, args.slice(0, this.__evN__.length)));
      remargs = args.slice(this.__evN__.length);
%%[[8
      trace("< _F_.__aN__(>sat)", fun + "(|needs#" + this.__evN__.length + "remargs#" + remargs.length + "=" + remargs + "|)");
      trace("<   ->", fun);
%%][100
%%]]
      return {
        __eOrV__ : function () {
          return fun.__aN__(remargs);
        }
      };
    }
  },
  needsNrArgs : function () {
    return this.__evN__.length;
  },
%%[[8
  getName : function () {
    return "F" + this.__evN__.length + "#" + this.nodeId + "'" + this.name;
  },
  toString : function () {
    return "(" + this.getName() + ")";
  }
%%][100
%%]]
}
%%]

// function construction wrappers
function _f_(f) {
  return new _F_(f);
}

// strict application wrappers
function _e1_(f, a) {
  return _e_(f.__aN__([a]));
}

function _e2_(f, a, b) {
  return _e_(f.__aN__([a, b]));
}

function _e3_(f, a, b, c) {
  return _e_(f.__aN__([a, b, c]));
}

function _e4_(f, a, b, c, d) {
  return _e_(f.__aN__([a, b, c, d]));
}

function _e5_(f, a, b, c, d, e) {
  return _e_(f.__aN__([a, b, c, d, e]));
}

function _eN_(f, a) {
  return _e_(f.__aN__(a));
}

%%[8
// lazy application wrappers
function _a0_(f) {
  return new _A_(f, []);
}
%%]

function _a1_(f, a) {
  return new _A_(f, [a]);
}

function _a2_(f, a, b) {
  return new _A_(f, [a, b]);
}

function _a3_(f, a, b, c) {
  return new _A_(f, [a, b, c]);
}

function _a4_(f, a, b, c, d) {
  return new _A_(f, [a, b, c, d]);
}

function _a5_(f, a, b, c, d, e) {
  return new _A_(f, [a, b, c, d, e]);
}

function _aN_(f, a) {
  return new _A_(f, a);
}

%%[8
// indirection
function _i_() {
  return new _A_(new _F_(
%%[[8
    "_i_",
%%][100
%%]]
    function () {throw "_i_: attempt to prematurely evaluate indirection"; }
  ), []);
}

function _i_set_(i, x) {
  i.__eOrV__ = x;
}
%%]

// setup
function init() {
}

%%[8
if (typeof document !== 'object') {
  document = {
    write   : function (x) {return write(x); },
    writeln : function (x) {return writeln(x); }
  };
};
%%]

function cons(x, y) { return [0, x, y]; }
function head(l) { return l[1]; }
function tail(l) { return l[2]; }
var nil = [1];
function isNil(x) { return x[0] === 1; }


function show(x) {
  document.write("" + _e_(x));
}

function showList(l) {
  var list = _e_(l);
  switch (list[0]) {
  case 0:
    document.write(_e_(head(list)) + ":");
    showList(tail(list));
    break;
  case 1:
    document.write("[]");
    break;
  }
}

// test: sieve
function testSieve() {
  var id, even, eq, ne, add, sub, mul, div, mod, from, last, take, filter,
    notMultiple, notMultiple2, sieve, sieve2, mainSieve, mainSieve2, d, t1, t2;
  id = _f_(function (a) {
    // trace("id: " + a);
    return a;
  });
  even = _f_(function (a) {
    // return __e__(a[0]) % 2 === 0;
    return _a2_(eq, _a2_(mod, a, 2), 0);
  });
  eq = _f_(function (a, b) {
    return _e_(a) === _e_(b);
  });
  ne = _f_(function (a, b) {
    return _e_(a) !== _e_(b);
  });
  add = _f_(function (a, b) {
    return _e_(a) + _e_(b);
  });
  sub = _f_(function (a, b) {
    return _e_(a) - _e_(b);
  });
  mul = _f_(function (a, b) {
    return _e_(a) * _e_(b);
  });
  div = _f_(function (a, b) {
    return Math.floor(_e_(a) / _e_(b));
  });
  mod = _f_(function (a, b) {
    return (_e_(a) % _e_(b));
  });
  from = _f_(function (a) {
    return cons(a, _a1_(from, _a2_(add, a, 1)));
  });
  last = _f_(function (a) {
    var list, list2;
    list = _e_(a);
    switch (list[0]) {
    case 0:
      list2 = _e_(tail(list));
      switch (list2[0]) {
      case 0:
        return _a1_(last, tail(list));
      case 1:
        return head(list);
      }
      break;
    case 1:
      return undefined;
    }
  });
  take = _f_(function (a, b) {
    var len, list;
    len  = _e_(a);
    list = _e_(b);
    if (len <= 0 || isNil(list)) {
      return nil;
    } else {
      return cons(head(list), _a2_(take, _a2_(sub, len, 1), tail(list)));
    }
  });
  filter = _f_(function (a, b) {
    var list, test;
    list = _e_(b);
    test = _e1_(a, head(list));
    if (test) {
      return cons(head(list), _a2_(filter, a, tail(list)));
    } else {
      return _a2_(filter, a, tail(list));
    }
  });
  notMultiple = _f_(function (a, b) {
    return _a2_(ne, _a2_(mul, _a2_(div, b, a), a), b);
  });
  notMultiple2 = _f_(function (a, b) {
    var x, y;
    x = _e_(a);
    y = _e_(b);
    return (Math.floor(y / x) * x) !== y;
  });
  sieve = _f_(function (a) {
    var list = _e_(a);
    return cons(head(list), _a1_(sieve, _a2_(filter, _a1_(notMultiple2, head(list)), tail(list))));
  });
  sieve2 = _f_(function (nmz, a) {
    var list = _e_(a);
    return cons(head(list), _a2_(sieve2, _a1_(id, nmz), _a2_(filter, _a1_(nmz, head(list)), tail(list))));
  });
  mainSieve = _a2_(take, 1000, _a1_(sieve, _a1_(from, 2)));
  mainSieve2 = _a2_(take, 500, _a2_(sieve2, _a1_(id, notMultiple2), _a1_(from, 2)));

  // running it...
  evalCounter = 0;
  d = new Date();
  t1 = d.getTime();
  // showList(mainSieve);
  show(_a1_(last, mainSieve));
  d = new Date();
  t2 = d.getTime() - t1;
  document.write("<hr/>time= " + t2 + " ms" + ((evalCounter > 0) ? ", nreval= " + evalCounter + ", ms/ev= " + (t2 / evalCounter) : "") + "<br/>");
}

function testMisc() {
  var plus, inc1, inc2, two1, two3, arr, x1, x2;
  trace("load & init ok");
  plus = _f_(function (a, b) {return _e_(a) + _e_(b); });
  trace("plus: " + plus);
  inc1 = _f_(function (a) {trace("inc: " + a); var x = _e_(a); return x + 1; });
  trace("inc1: " + inc1);
  inc2 = plus.__aN__([10]);
  trace("inc2: " + inc2);
  two1 = 2;
  // var two2 = new AppN_WHNF(2);
  two3 = new _A_(new _F_(0,function () { return 2; }), []);
  arr = [two1];
  // trace("two2: " + two2);
  trace("two3: " + two3);
  trace("two3 eval: " + _e_(two3));
  trace("two3: " + two3);
  trace("two3 eval: " + _e_(two3));
  trace("arr: " + arr);
  x1 = inc2.__aN__(arr);
  trace("inc 2: " + x1);
  x2 = new _A_(inc2, arr);
  trace("inc del 2: " + x2);
  trace("inc del 2 eval: " + _e_(x2));
}

function tryOut() {
  var f, l;
  f = function (a, b) {};
  l = cons(1, nil);
  // trace(ToPropertyDescriptor(f));
  // trace(ToPropertyDescriptor(Function));
  // trace(ToPropertyDescriptor("a"));
  // trace(ToPropertyDescriptor(String));
  trace("f " + f.length);
}

function main() {
  init();
  // testMisc();
  // tryOut();
  testSieve();
}
