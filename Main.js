// Generated by psc-bundle 0.11.6
var PS = {};
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Category = PS["Control.Category"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["const"] = $$const;
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
    "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Data.Unit"];
  var Data_Show = PS["Data.Show"];
  exports["unit"] = $foreign.unit;
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Data.Functor"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Function = PS["Data.Function"];
  var Data_Unit = PS["Data.Unit"];        
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];        
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["liftA1"] = liftA1;
  exports["pure"] = pure;
})(PS["Control.Applicative"] = PS["Control.Applicative"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
    "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Control.Monad.Eff"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad = PS["Control.Monad"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var monadEff = new Control_Monad.Monad(function () {
      return applicativeEff;
  }, function () {
      return bindEff;
  });
  var bindEff = new Control_Bind.Bind(function () {
      return applyEff;
  }, $foreign.bindE);
  var applyEff = new Control_Apply.Apply(function () {
      return functorEff;
  }, Control_Monad.ap(monadEff));
  var applicativeEff = new Control_Applicative.Applicative(function () {
      return applyEff;
  }, $foreign.pureE);
  var functorEff = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEff));
  exports["functorEff"] = functorEff;
  exports["applyEff"] = applyEff;
  exports["applicativeEff"] = applicativeEff;
  exports["bindEff"] = bindEff;
  exports["monadEff"] = monadEff;
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Category = PS["Control.Category"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Monad = PS["Control.Monad"];
  var Control_MonadZero = PS["Control.MonadZero"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var Prelude = PS["Prelude"];        
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
})(PS["Data.Maybe"] = PS["Data.Maybe"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.setFillStyle = function(style) {
      return function(ctx) {
          return function() {
              ctx.fillStyle = style;
              return ctx;
          };
      };
  };

  exports.beginPath = function(ctx) {
      return function() {
          ctx.beginPath();
          return ctx;
      };
  };

  exports.fill = function(ctx) {
      return function() {
          ctx.fill();
          return ctx;
      };
  };

  exports.arc = function(ctx) {
      return function(a) {
          return function() {
              ctx.arc(a.x, a.y, a.r, a.start, a.end);
              return ctx;
          };
      };
  };

  exports.rotate = function(angle) {
      return function(ctx) {
          return function() {
              ctx.rotate(angle);
              return ctx;
          };
      };
  };

  exports.translate = function(t) {
      return function(ctx) {
          return function() {
              ctx.translate(t.translateX, t.translateY);
              return ctx;
          };
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Graphics.Canvas"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Exception_Unsafe = PS["Control.Monad.Eff.Exception.Unsafe"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Function_Uncurried = PS["Data.Function.Uncurried"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Prelude = PS["Prelude"];
  var getCanvasElementById = function (elId) {
      return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
  };
  var fillPath = function (ctx) {
      return function (path) {
          return function __do() {
              var v = $foreign.beginPath(ctx)();
              var v1 = path();
              var v2 = $foreign.fill(ctx)();
              return v1;
          };
      };
  };
  exports["fillPath"] = fillPath;
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["arc"] = $foreign.arc;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["rotate"] = $foreign.rotate;
  exports["setFillStyle"] = $foreign.setFillStyle;
  exports["translate"] = $foreign.translate;
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
    "use strict";              

  exports.pi = Math.PI;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Math"];
  exports["pi"] = $foreign.pi;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
    "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Unit = PS["Data.Unit"];
  var Flare = PS["Flare"];
  var Graphics_Canvas = PS["Graphics.Canvas"];
  var Graphics_Drawing = PS["Graphics.Drawing"];
  var Graphics_Isometric = PS["Graphics.Isometric"];
  var $$Math = PS["Math"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Prelude = PS["Prelude"];
  var Signal_DOM = PS["Signal.DOM"];        

  /**
 * -makeSemiCircle1 ctx-
 */  

  /**
 * -makeSemiCircle2 ctx-
 */  
  var rotateLogo = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
      var v = Graphics_Canvas.getCanvasElementById("canvas")();
      var __unused = function (dictPartial1) {
          return function ($dollar9) {
              return $dollar9;
          };
      };
      return __unused()((function () {
          if (v instanceof Data_Maybe.Just) {
              return function __do() {
                  var v1 = Graphics_Canvas.getContext2D(v.value0)();
                  Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.translate({
                      translateX: 50.0, 
                      translateY: 50.0
                  })(v1))();
                  Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.rotate(1.5 * $$Math.pi)(v1))();
                  return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.translate({
                      translateX: -50.0, 
                      translateY: -50.0
                  })(v1))();
              };
          };
          throw new Error("Failed pattern match at ArcTest line 69, column 2 - line 70, column 2: " + [ v.constructor.name ]);
      })())();
  });
  var makeSemiCircle2 = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
      var v = Graphics_Canvas.getCanvasElementById("canvas")();
      var __unused = function (dictPartial1) {
          return function ($dollar13) {
              return $dollar13;
          };
      };
      return __unused()((function () {
          if (v instanceof Data_Maybe.Just) {
              return function __do() {
                  var v1 = Graphics_Canvas.getContext2D(v.value0)();
                  Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.setFillStyle("#ffffff")(v1))();
                  return Graphics_Canvas.fillPath(v1)(Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.arc(v1)({
                      x: 250.0, 
                      y: 240.0, 
                      r: 50.0, 
                      start: 0.5 * $$Math.pi, 
                      end: 1.5 * $$Math.pi
                  })))();
              };
          };
          throw new Error("Failed pattern match at ArcTest line 47, column 2 - line 48, column 2: " + [ v.constructor.name ]);
      })())();
  });
  var makeSemiCircle1 = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
      var v = Graphics_Canvas.getCanvasElementById("canvas")();
      var __unused = function (dictPartial1) {
          return function ($dollar17) {
              return $dollar17;
          };
      };
      return __unused()((function () {
          if (v instanceof Data_Maybe.Just) {
              return function __do() {
                  var v1 = Graphics_Canvas.getContext2D(v.value0)();
                  Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.setFillStyle("#ffffff")(v1))();
                  return Graphics_Canvas.fillPath(v1)(Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.arc(v1)({
                      x: 250.0, 
                      y: 260.0, 
                      r: 50.0, 
                      start: 1.5 * $$Math.pi, 
                      end: 0.5 * $$Math.pi
                  })))();
              };
          };
          throw new Error("Failed pattern match at ArcTest line 34, column 2 - line 35, column 2: " + [ v.constructor.name ]);
      })())();
  });
  var makeCircle = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
      var v = Graphics_Canvas.getCanvasElementById("canvas")();
      var __unused = function (dictPartial1) {
          return function ($dollar21) {
              return $dollar21;
          };
      };
      return __unused()((function () {
          if (v instanceof Data_Maybe.Just) {
              return function __do() {
                  var v1 = Graphics_Canvas.getContext2D(v.value0)();
                  Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.setFillStyle("#0000FF")(v1))();
                  return Graphics_Canvas.fillPath(v1)(Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.arc(v1)({
                      x: 250.0, 
                      y: 250.0, 
                      r: 100.0, 
                      start: 0.0, 
                      end: 360.0
                  })))();
              };
          };
          throw new Error("Failed pattern match at ArcTest line 20, column 2 - line 21, column 2: " + [ v.constructor.name ]);
      })())();
  });
  var drawLogo = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
      makeCircle();
      makeSemiCircle1();
      return makeSemiCircle2();
  });
  var main = function __do() {
      drawLogo();
      rotateLogo();
      drawLogo();
      rotateLogo();
      return Data_Unit.unit;
  };
  exports["drawLogo"] = drawLogo;
  exports["main"] = main;
  exports["makeCircle"] = makeCircle;
  exports["makeSemiCircle1"] = makeSemiCircle1;
  exports["makeSemiCircle2"] = makeSemiCircle2;
  exports["rotateLogo"] = rotateLogo;
})(PS["ArcTest"] = PS["ArcTest"] || {});
PS["ArcTest"].main();
