(function() {
  var Out;
  var __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };
  Out = (function() {
    var hasConsoleError, hasConsoleLog;
    function Out() {
      this.ut = __bind(this.ut, this);
      this.log = __bind(this.log, this);
      this.ut_error = __bind(this.ut_error, this);      this.test = false;
      this.verbose = true || this.test;
    }
    hasConsoleLog = (typeof console !== "undefined" && console !== null ? console.log : void 0) != null;
    hasConsoleError = (typeof console !== "undefined" && console !== null ? console.error : void 0) != null;
    Out.prototype.has_console_log = function() {
      return hasConsoleLog;
    };
    Out.prototype.has_console_error = function() {
      return hasConsoleError;
    };
    Out.prototype.ut_error = function(txt) {
      var has_e;
      has_e = this.has_console_error();
      if (has_e) {
        console.error(txt);
      }
      return has_e;
    };
    Out.prototype.log = function(txt) {
      var has_l;
      has_l = this.has_console_log();
      if (has_l && this.verbose) {
        console.log(txt);
      }
      return has_l;
    };
    Out.prototype.ut = function(talky, name, txt) {
      if ((talky != null) && talky === true) {
        this.log("\n");
        this.log(name);
        if (txt != null) {
          this.log(txt);
        }
      }
      return this.has_console_log();
    };
    return Out;
  })();
  window.o = new Out();
}).call(this);
