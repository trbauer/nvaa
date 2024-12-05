////////////////////////////////////////////////////////////////////////////////
// Adapted from github.com/trbauer
//
// A simple lexical scanner/parser
function parser(inp) {
  this.input = inp
  this.off = 0

  this.eof = function() {return this.off >= this.input.length}
  this.offset = function() {return this.off}
  this.suffix = function() {return this.input.substr(this.off)}
  this.skip = function(n) {this.off = Math.min(this.off + n, this.input.length)}
  this.skipWs = function() {
    while (!this.eof() && /\s/.test(this.input[this.off])) {
      this.skip(1)
    }
  }
  //////////////////////////////////////////////////////////////////////////////
  this.errorAt = function(off, msg) {
    let s = msg + "\n" + this.input + "\n"
    for (let i = 0; i < off; i++)
      s += ' '
    s += '^'
    throw Error(s)
  }
  this.error = function(msg) {this.errorAt(this.off, msg)}
    //////////////////////////////////////////////////////////////////////////////
  this.lookingAt = function(pfx) {return this.input.indexOf(pfx, this.off) == this.off}
  this.lookingAtInt = function(pfx) {
    let o = this.off
    let i = this.consumeIfIntNT()
    this.off = o
    return i != null
  }

  //////////////////////////////////////////////////////////////////////////////
  this.consumeIfNT = function(pfx) {
    if (this.lookingAt(pfx)) {
      this.skip(pfx.length)
      return true
    }
    return false
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeIf = function(pfx) {
    if (this.consumeIfNT(pfx)) {
      this.skipWs()
      return true
    }
    return false
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeIfOneOf = function(pfxs) {
    pfxs.sort((a, b) => b.length - a.length);
    for (let i = 0; i < pfxs.length; i++) {
      if (this.lookingAt(pfxs[i])) {
        this.skip(pfxs[i].length)
        this.skipWs()
        return i;
      }
    }
    return null;
  }
  this.consumeOneOf = function(pfxs) {
    const ix = this.consumeIfOneOf(pfxs)
    if (ix == null) {
      this.error("expected one of " + pfxs.join(", "));
    }
    return ix
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeIfInt10NT = function(pfx) {
    var i = 0
    function isDecChar(c) {return c >= '0' && c <= '9'}
    if (this.eof() || !isDecChar(this.input[this.off + i])) {
      return null
    }
    i++
    while (this.off + i < this.input.length &&
        (isDecChar(this.input[this.off + i])))
    {
      i++
    }
    var s = this.input.substr(this.off, i)
    this.skip(i)
    return parseInt(s)
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeIfInt16NT = function(pfx) {
    var i = 0
    if (!this.lookingAt("0x") && !this.lookingAt("0X")) {
      return null
    }
    i += 2
    function isHexChar(c) {
      return c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'
    }
    if (this.off + i >= this.input.length || !isHexChar(this.input[this.off + i])) {
      return null
    }
    i++;
    while (this.off + i < this.input.length && isHexChar(this.input[this.off + i]))
      i++;
    var s = this.input.substr(this.off, i)
    this.skip(i)
    return parseInt(s)
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeIfIntNT = function() {
    let x = this.consumeIfInt16NT()
    if (x == null)
      x = this.consumeIfInt10NT()
    return x
  }
  this.consumeIfSignedIntNT = function() {
    const off = this.off
    const sign = this.consumeIf("-") ? -1 : 1
    const x = this.consumeIfInt()
    if (x == null)
      this.off = off
    return sign * x
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeIfInt = function() {
    const x = this.consumeIfIntNT()
    if (x != null)
      this.skipWs()
    return x
  }
  this.consumeIfSignedInt = function() {
    const x = this.consumeIfSignedIntNT()
    if (x != null)
      this.skipWs()
    return x
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeNT = function(pfx) {
    if (!this.consumeIf(pfx)) {
      this.error("expected " + pfx)
    }
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consume = function(pfx) {
    this.consumeNT(pfx)
    this.skipWs()
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeInt16NT = function() {
    const i = this.consumeIfInt16NT()
    if (i == null)
      this.error("expected (hex) int")
    return i
  }
  this.consumeInt10NT = function() {
    const i = this.consumeIfInt10NT()
    if (i == null)
      this.error("expected int")
    return i
  }
  this.consumeIntNT = function() {
    let i = this.consumeIfInt16NT()
    if (i == null)
      i = this.consumeIfInt10NT()
    if (i == null)
      this.error("expected int")
    return i
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeInt = function() {
    const i = this.consumeIntNT()
    this.skipWs()
    return i
  }
  this.consumeInt10 = function() {
    const i = this.consumeInt10NT()
    this.skipWs()
    return i
  }
  this.consumeInt16 = function() {
    const i = this.consumeInt16NT()
    this.skipWs()
    return i
  }
  this.consumeSignedInt = function() {
    const sign = this.consumeIf("-") ? -1 : 1
    return sign * this.consumeInt()
  }
  //////////////////////////////////////////////////////////////////////////////
  this.consumeIntSeq = function() {
    const seq = []
    //
    function parseSeg(p) {
      if (p.consumeIf("aseq")) {
        // arithmetic sequence aseq(base,delta)^repeat
        p.consume("(")
        const base = p.consumeSignedInt()
        p.consume(",")
        const delta = p.consumeSignedInt()
        p.consume(")")
        p.consume("^")
        const repeat = p.consumeSignedInt()
        for (let i = 0; i < repeat; i++) {
          seq.push(base + i * delta)
        }
        return true
      } else if (p.consumeIf("[")) {
        // literal list [1,2,3,4]
        let lit = []
        if (!p.lookingAt("]")) {
          lit.push(p.consumeSignedInt())
          while (p.consumeIf(","))
            lit.push(p.consumeSignedInt())
        }
        p.consume("]")
        let ncopy = lit.length
        if (p.consumeIf("^")) {
          ncopy = p.consumeSignedInt()
        }
        for (let r = 0; r < ncopy; r++) {
          seq.push(lit[r % lit.length])
        }
      return true
      } else {
        // constant sequence (const^repeat)
        const konst = p.consumeIfSignedInt()
        if (konst == null) {
          return false
        }
        p.consume("^")
        const repeat = p.consumeSignedInt()
        for (let i = 0; i < repeat; i++) {
          seq.push(konst)
        }
        return true
      }
    } // parseSeg

    if (!parseSeg(this))
      this.error("expected int sequence")
    while (this.consumeIf("+"))
      if (!parseSeg(this))
        this.error("expected int sequence")
    return seq
  }
} // parser

// breaks a list of numbers into a segments of arithmetic sequences
// with a length count on each.
// * An arithmetic sequence is of the form: 'aseq(base,delta)^length' with length > 1
// * We simplify constant sequences (delta == 0) as just 'const^length'
// * Segments are separated by a plus symbol (+).
//
// SPECIFY: (under consideration)
// * A .. pads out the last segment to the end (the length is implied).
// * Minimum sequence length is an internal constant; if there is nothing
//   then we print a list literal (e.g. [1,2])
//   (use a junk list to maximally group literals)
//
// EXAMPLES:
//  format_sequence([0,4,8,0,0])   ==> 'aseq(0,4)^3+0^2'
//  format_sequence([0,0])         ==> '0^2'
//  format_sequence([0,0,4,8])     ==> '0^2+aseq(4,4)^2'
//     (sequences are left-to-right greedy)
//  format_sequence([0])           ==> '0^1'
//  format_sequence([0,1,2,5,3,4]) ==> 'aseq(0,1)^3+aseq(5,-2)^2+4^1'
//  format_sequence([0,0,1,1])     ==> '0^2+1^2'
function format_sequence(seq, radix_) {
  // console.log("format_sequence", seq)
  // const MIN_ASEQ_LEN = 3
  const radix = radix_ || 16
  function seqlen_from(i) {
    let k = i
    if (k >= seq.length - 1)
      return 1
    let delta = seq[k + 1] - seq[k]
    while (k < seq.length && seq[k] + delta == seq[k + 1]) {
      k++
    }
    return k + 1 - i
  }
  function fmt_num(x) {
    let pfx = radix == 16 ? "0x" : ""
    if (x < 0) {
      pfx = "-" + pfx
      x = -x
    }
    return pfx + x.toString(radix).toUpperCase()
  }

  let i = 0
  let s = ""
  while (i < seq.length) {
    if (i == seq.length - 1) {
      s += fmt_num(seq[i]) + "^" + fmt_num(1)
      break
    }
    const len = seqlen_from(i)
    const base = seq[i]
    const delta = seq[i + 1] - seq[i]
    if (delta == 0) {
      s += fmt_num(base) + "^" + fmt_num(len)
    } else {
      s += "aseq(" + fmt_num(base) + "," + fmt_num(delta) + ")^" + fmt_num(len)
    }
    i += len
    if (i < seq.length) {
      s += "+"
    }
  }
  return s
}

////////////////////////////////////////////////////////////////////////////////
// quick dirty unit tests for integer sequences
function test_sequences()
{
  function test(t_seq, t_fmt, t_radix) { // t_radix can be undefined
    return {seq:t_seq, fmts_as:t_fmt, radix:t_radix}
  }

  // TODO: literal lists [1,2] (formats otherwise)
  const TESTS =
    [
      test([0], '0x0^0x1', 16),
      test([0], '0^1', 10),
      test([0,0], '0x0^0x2',16),
      test([0,0,4,8], '0x0^0x2+aseq(0x4,0x4)^0x2'),
      test([0], '0x0^0x1'),
      test([-5,-7],"aseq(-5,-2)^2",10),
      test([-5,-7],"aseq(-0x5,-0x2)^0x2",16),
      test([0,4,8,0,0], 'aseq(0x0,0x4)^0x3+0x0^0x2'),
      test([0,0,1,1], '0x0^0x2+0x1^0x2', 16),
      test([0,0,1,1], '0^2+1^2', 10),
      test([0,4], 'aseq(0,4)^2', 10),
      test([0,4], 'aseq(0x0,0x4)^0x2', 16),
    ]
  let passed = 0
  function arrays_equal(a1, a2) {
    return a1.length === a2.length && a1.every((value, index) => value === a2[index])
  }
  for (let i = 0; i < TESTS.length; i++) {
    const t = TESTS[i]
    const t_seq = t.seq
    const t_fmt_exp  = t.fmts_as
    const t_radix = t.radix
    let t_parsed = null
    try {
      t_parsed = new parser(t_fmt_exp).consumeIntSeq()
    } catch (e) {
      console.log("ERROR parsing: ", t_fmt_exp)
      console.log(e.stack)
    }
    const t_fmt_actual = format_sequence(t_seq, t_radix)
    if (t_fmt_exp != t_fmt_actual) {
      console.log("test_sequences failed to:",
          "format_sequence(", t_seq, "); expected: ", t_fmt_exp, "; actual: ", t_fmt_actual)
    } else if (!arrays_equal(t_parsed, t_seq)) {
      console.log("test_sequences failed to:",
          "new parser(t_fmt).consumeIntSeq(", t_fmt_exp, ")", "; expected: ", t_seq, "; actual: ", t_parsed)
    } else {
      passed++
    }
  }
  console.log("test_format_sequence passed: ", passed, "of", TESTS.length)
}

////////////////////////////////////////////////////////////////////////////////
const is_pow2 = {what:"power of two", func:function(x){(x & (x - 1)) === 0}}

function check_is_bool(what, x) {
  if (typeof(x) != "boolean") {
    throw Error(what + " is not an boolean")
  }
}
function check_is_int(what, x) {
  if (!Number.isInteger(x)) {
    throw Error(what + " is not an integer")
  }
}
function check_is_int_array_of_length(what, arr, n) {
  if (!Array.isArray(arr)) {
    console.log(arr)
    throw Error(what + ": must be an array")
  }
  if (arr.length != n) {
    console.log(arr)
    throw Error(what + ": wrong number of elements in int array (expected " + n + ")")
  }
  arr.forEach((e,ix) => {
    if (!Number.isInteger(e)) {
      throw Error(what + "[" + ix + "] is not an integer")
    }
  })
  return arr
}
function check_is_int(what, x, check) {
  if (!Number.isInteger(x)) {
    throw Error(what + " is not an integer")
  }
  if (check) {
    if (!check.func(x)) {
      throw Error(what + " must be " + check.what)
    }
  }
  return x
}

function aseq(base, delta, reps) {
  const xs = []
  for (let i = 0; i < reps; i++)
    xs.push(base + i * delta)
  return xs
}

////////////////////////////////////////////////////////////////////////////////
function fmt_hex(x,width) {
  if (Array.isArray(x)) {
    let s = "["
    for (let i = 0; i < x.length; i++) {
      if (i != 0)
        s += ","
      s += fmt_hex(x[i])
    }
    return s + "]"
  }
  function pad(s, width) {
    if (width == undefined)
      return s
    while (s.length < width)
      s = '0' + s
    return s
  }
  return x == undefined ? "undefined" :
         x == null ? "null" :
         !Number.isInteger(x) ? x.toString() :
         x < 0 ? "-0x" + pad((-x).toString(16).toUpperCase(),width) :
         "0x" + pad(x.toString(16).toUpperCase(),width)
}

function fmt_dec(x) {
  if (Array.isArray(x)) {
    let s = "["
    for (let i = 0; i < x.length; i++) {
      if (i != 0)
        s += ","
      s += fmt_dec(x[i])
    }
    return s + "]"
  }
  return x == undefined ? "undefined" :
         x == null ? "null" :
         x.toString()
}

////////////////////////////////////////////////////////////////////////////////
function div_mod(x, m)
{
  if (!Number.isInteger(x)) throw new Error("div_mod on non-integer")
  if (!Number.isInteger(m)) throw new Error("div_mod on non-integer")
  return [Math.floor(x / m), x % m]
}

function round_up_pow2(x) {
  while ((x & (x - 1)) != 0) {
    x++
  }
  return x
}

function round_to_multiple_of(a, x) {
  return (x + a - 1) - ((x + a - 1) % a);
}