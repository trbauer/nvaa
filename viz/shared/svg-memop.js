////////////////////////////////////////////////////////////////////////////////
// Adapted from github.com/trbauer
"use strict";

// product type to represent a byte mapping
function byte_mapping(lane, mem_byte, grf, grf_byte_offset, color, zero_fill) {
  zero_fill = zero_fill || false
  this.lane = lane // the lane that loads this byte
  if (!Number.isInteger(mem_byte)) {
    throw new Error("mem_byte must be an integer")
  }
  this.mem_byte = mem_byte // the memory address this points to (null if a zero fill or something similar)
  if (!Number.isInteger(grf)) {
    throw new Error("grf must be an integer")
  }
  this.grf = grf // dst GRF index
  if (!Number.isInteger(grf_byte_offset)) {
    throw new Error("grf_byte_offset must be an integer")
  }
  this.grf_byte_offset = grf_byte_offset // dst GRF offset (infer dst lane by this)
  if (typeof(color) != 'string') {
    throw new Error("color must be a string")
  }
  this.color = color
  if (typeof(zero_fill) != 'boolean') {
    throw new Error("zero_fill must be a boolean")
  }
  this.zero_fill = zero_fill == true
  this.mem = function() {
    return (this.zero_fill ? "zero-filled after " : "") +
        "mem[" + fmt_hex(this.mem_byte, 3) + "]"
  }
  this.reg = function() {return "dst[" + grf + "][" + fmt_hex(this.grf_byte_offset, 3) + "]"}
}

////////////////////////////////////////////////////////////////////////////////
// render memory
//
//  svg - is this 'svg_builder' object created
//  svg_cfg - config with
//  mem_contents - the Map mapping linear byte index to 'byte' object
//  mem_w, mem_h - the memory dimensions to render
//
// Assumes svg_builder's pen is in the top-left corner of the draw area and
// advances it.
function render_memory(svg, svg_cfg, mem_contents, mem_w, mem_h) {
  if (!Number.isInteger(svg_cfg.text_h))
    throw Error("invalid param")
  if (!Number.isInteger(svg_cfg.px_per_byte))
    throw Error("invalid param")
  if (!Number.isInteger(svg_cfg.text_ind))
    throw Error("invalid param")
  for (let y = 0; y < mem_h; y++) {
    svg.set_pen_x(svg_cfg.ind)
    svg.text("mem[" + fmt_hex(mem_w * y, 3) + "]", "monospace", svg_cfg.text_h, svg_cfg.text_ind)
    for (let x = 0; x < mem_w; x++) {
      const mem_off = mem_w * y + x
      const bs = mem_contents.get(mem_off)
      let b_color = ""
      let b_hint = ""
      if (bs.length == 0) {
        b_color = color_no_mapping
      } else if (bs.length == 1) {
        b_color = bs[0].color
        b_hint = bs[0].reg()
      } else {
        b_color = color_conflict
        b_hint = "conflicts between:\n"
        bs.forEach(function(b){b_hint += "  " + b.reg() +"\n"})
      }
      svg.rect("m." + mem_off, svg_cfg.px_per_byte, svg_cfg.text_h, b_color,
               "mem[" + fmt_hex(mem_off,3) + "] = " + b_hint)
    } // x
    svg.add_pen_y(svg_cfg.text_h)
  } // y
}

////////////////////////////////////////////////////////////////////////////////
// Renders the registers
//   svg - is this 'svg_builder' object created (positioned at top left of area / we advance it here)
//   reg_contents - the register contents Array(Array([byte])) with each byte list of bytes
//                  mapping to the mapped bytes from memory that hit there
//   bytes_per_register - the count of bytes per register per warp (e.g. 128 = SIMT32 * 4)
function render_registers(svg, svg_cfg, reg_contents, bytes_per_reg) {
  if (!Number.isInteger(bytes_per_reg))
    throw Error("bytes_per_reg: invalid param")
  if (!Number.isInteger(svg_cfg.text_h))
    throw Error("svg_cfg.text_h: invalid param")
  if (!Number.isInteger(svg_cfg.px_per_byte))
    throw Error("svg_cfg.px_per_byte: invalid param")
  if (!Number.isInteger(svg_cfg.text_ind))
    throw Error("svg_cfg.text_ind: invalid param")

  const lane_ind = 8
  svg.add_pen_y(svg_cfg.ind)
  svg.set_pen_x(svg_cfg.ind + svg_cfg.text_ind)
  for (let reg_off = 0; reg_off < bytes_per_reg; reg_off += 4) {
    svg.text("lane[" + (reg_off / 4) + "]", "monospace", svg_cfg.text_h, 0)
    svg.add_pen_x(4 * svg_cfg.px_per_byte + lane_ind)
  }

  svg.add_pen_y(svg_cfg.ind)
  for (let dst = 0; dst < reg_contents.length; dst++) {
    svg.set_pen_x(svg_cfg.ind)
    svg.text("dst[" + dst + "]", "monospace", svg_cfg.text_h, svg_cfg.text_ind)
    for (let reg_off = 0; reg_off < bytes_per_reg; reg_off++) {
      // find all that map to this byte
      const r_id = "r." + dst + "." + reg_off
      const mem_srcs = reg_contents[dst][reg_off]
      if (mem_srcs.length == 0) {
        svg.rect(r_id, svg_cfg.px_per_byte, svg_cfg.text_h, color_no_mapping, "no mapping")
      } else if (mem_srcs.length > 1) {
        let mm = ""
        mem_srcs.forEach(function(b){mm += b.mem() +"\n"})
        svg.rect(r_id, svg_cfg.px_per_byte, svg_cfg.text_h, color_conflict, "multiple mappings\n" + mm)
      } else {
        svg.rect(r_id, svg_cfg.px_per_byte, svg_cfg.text_h, mem_srcs[0].color, mem_srcs[0].mem())
      }
      if (reg_off % 4 == 3)
        svg.add_pen_x(lane_ind)
    }
    svg.set_pen_x(svg_cfg.ind)
    svg.add_pen_y(svg_cfg.text_h)
  }
}

////////////////////////////////////////////////////////////////////////////////
// after the SVG is created this function binds mouse events to the SVG elements
function bind_listeners(mem_contents, reg_contents, to_id) {
  let pinned = false
  for (let [mem_off,bs] of mem_contents) {
    const mid = to_id("m." + mem_off)
    const m_elem = document.getElementById(mid)
    if (m_elem == null) {
      console.log("mapping mouse events m_elem(" + mid + ") is null!")
      continue
    }
    if (bs.length > 0)
      m_elem["r_elems"] = []
    for (let b of bs) {
      if (b.grf != null && b.grf_byte_offset != null) {
        const r_id = to_id("r." + b.grf + "." + b.grf_byte_offset)
        const r_elem = document.getElementById(r_id)
        if (r_elem == null) {
          console.log("mapping mouse events r_elem(" + r_id + ") is null!")
          continue
        }
        m_elem["r_elems"].push(r_elem)
      }
    }
    m_elem.addEventListener("mouseenter", function(e) {
      if (pinned) {
        return
      }
      e.currentTarget.style.stroke = "white"
      if (e.currentTarget.r_elems) {
        e.currentTarget.r_elems.forEach(function(r_elem) {
          r_elem.style.stroke = "white"
        })
      }
    })
    m_elem.addEventListener("mouseleave", function(e) {
      if (pinned) {
        return
      }
      e.currentTarget.style.stroke = "black"
      if (e.currentTarget.r_elems) {
        e.currentTarget.r_elems.forEach(function(r_elem) {
          r_elem.style.stroke = "black"
        })
      }
    })
    m_elem.addEventListener("click", function(e) {
      pinned = !pinned
    })
  } // for mem_contents
  for (let dst = 0; dst < reg_contents.length; dst++) {
    for (let reg_off = 0; reg_off < reg_contents[dst].length; reg_off++) {
      const r_id = to_id("r." + dst + "." + reg_off)
      const r_elem = document.getElementById(r_id)
      if (r_elem == null) {
        console.log("r_elem(" + r_id + ") == null")
        continue
      }
      r_elem.addEventListener("mouseenter", function(e) {
        if (pinned) {
          return
        }
        e.currentTarget.style.stroke = "white"
        const bs = reg_contents[dst][reg_off]
        bs.forEach(function(b) {
          if (b.mem_byte != null) {
            const m_elem = document.getElementById(to_id("m." + b.mem_byte))
            if (m_elem != null)
              m_elem.style.stroke = "white"
          }
        })
      })
      r_elem.addEventListener("mouseleave", function(e) {
        if (pinned) {
          return
        }
        e.currentTarget.style.stroke = "black"
        const bs = reg_contents[dst][reg_off]
        bs.forEach(function(b) {
          if (b.mem_byte != null) {
            const m_elem = document.getElementById(to_id("m." + b.mem_byte))
            if (m_elem != null)
              m_elem.style.stroke = "black"
          }
        })
      })
      r_elem.addEventListener("click", function(e) {
        pinned = !pinned
      })
    }
  } // for mem_contents
}

////////////////////////////////////////////////////////////////////////////////
function schema_get_field_id(field_name, our_id) {
  return "input-" + field_name + "-" + our_id
}

function schema_int(name, desc, width, fmt_int) {
  this.name = name
  this.desc = desc
  this.type = "int"
  this.width = width
  this.format = function(obj) {return fmt_int(obj[name])}
  this.get_control_value = function(html_elem) {return html_elem.value}
  this.parse = function(inp) {
      const p = new parser(inp)
      p.skipWs()
      const x = p.consumeSignedInt()
      if (!p.eof()) {
        p.error(this.name + ": unexpected trailing characters")
      }
      return x
    }
}

function schema_boolean(name, desc, width) {
  this.name = name
  this.desc = desc
  this.type = "boolean"
  this.width = width
  this.format = function(obj) {return obj[name] == true ? "true" : "false"}
  this.get_control_value = function(html_elem) {return html_elem.checked.toString()}
  this.parse = function(inp) {
      const p = new parser(inp)
      p.skipWs()
      let x = false
      if (p.consumeIf("true"))
        x = true
      else if (p.consumeIf("false"))
        x = false
      else
        p.error("expected true or false")
      if (!p.eof()) {
        p.error(this.name + ": unexpected trailing characters")
      }
      return x
    }
}

function full_mask(simt_lanes) {
  return fmt_hex(simt_lanes == 32 ? 0xFFFFFFFF : (1 << simt_lanes) - 1)
}

// can return null if empty
function schema_execmask(name, desc, width, fmt_int) {
  this.name = name
  this.desc = desc
  this.type = "execmask"
  this.width = width
  this.format = function(obj) {return fmt_int(obj[name])}
  this.get_control_value = function(html_elem) {return html_elem.value}
  this.parse = function(inp) {
      const p = new parser(inp)
      p.skipWs()
      if (p.eof()) {
        return null
      }
      const x = p.consumeSignedInt()
      if (!p.eof()) {
        p.error(this.name + ": unexpected trailing characters")
      }
      return x
    }
}

function schema_memsize(name, desc, width) {
  this.name = name
  this.desc = desc
  this.type = "memsize"
  this.width = width == undefined ? 48 : width
  this.format = function(obj) {return fmt_dec(obj[name].w) + "x" + fmt_dec(obj[name].h)}
  this.get_control_value = function(html_elem) {return html_elem.value}
  this.parse = function(inp) {
      const p = new parser(inp)
      p.skipWs()
      if (p.eof()) {
        return null
      }
      const w = p.consumeInt10NT()
      p.consumeNT("x")
      const h = p.consumeInt10()
      if (!p.eof()) {
        p.error(this.name + ": unexpected trailing characters")
      }
      return {w:w,h:h}
    }
}

function schema_datasize(name, desc, width) {
  this.name = name
  this.desc = desc
  this.type = "datasize"
  this.width = width
  this.format = function(obj) {
    const s = "D" + (8 * obj[name].mem_size)
    if (obj[name].zero_fill > 0)
      s += "U" + (8 * obj[name].zero_fill)
    return s
  }
  this.get_control_value = function(html_elem) {return html_elem.value}
  this.parse = function(inp) {
      const p = new parser(inp)
      p.skipWs()
      const d_off = p.offset()
      p.consumeNT('D')
      let data_size = p.consumeInt10NT()
      if (data_size != 8 && data_size != 16 && data_size != 32 &&
          data_size != 64 && data_size != 96 && data_size != 128 && data_size != 256)
      {
        p.errorAt(d_off, "invalid data size")
      }
      const u_off = p.offset()
      let zf = 0
      if (p.consumeIf("U32")) {
        if (data_size != 8 && data_size != 16) {
          p.errorAt(u_off, "...U32 only supported on D8 or D16")
        }
        zf = 4 - data_size / 8
      }
      if (!p.eof()) {
        p.error(this.name + ": unexpected trailing characters")
      }
      return {mem_size:data_size / 8,zero_fill:zf}
    }
}
function schema_vecsize(name, desc, width) {
  this.name = name
  this.desc = desc
  this.type = "vecsize"
  this.width = width
  this.format = function(obj) {return "V" + (8 * obj[name])}
  this.get_control_value = function(html_elem) {return html_elem.value}
  this.parse = function(inp) {
      const p = new parser(inp)
      p.skipWs()
      const v_off = p.offset()
      p.consumeNT('V')
      let v = p.consumeInt10()
      if (v != 1 && v != 2 && v != 3 && v != 4 && v != 8 && v != 16 && v != 32 && v != 64) {
        p.errorAt(d_off, "invalid vector size (V1...V64 powers of 2 and V3)")
      }
      if (!p.eof()) {
        p.error(this.name + ": unexpected trailing characters")
      }
      return v
    }
}

function schema_intseq(name, desc, width) {
  this.name = name
  this.desc = desc
  this.type = "intseq"
  this.width = width
  this.format = function(obj) {return format_sequence(obj[this.name])}
  this.get_control_value = function(html_elem) {return html_elem.value}
  this.parse = function(inp) {
      const p = new parser(inp)
      p.skipWs()
      const x = p.consumeIntSeq()
      if (!p.eof()) {
        p.error("unexpected " + what + " trailing characters")
      }
      return x
    }
}

// [["one",1],["two",2],["three",3]]
function schema_enum(name, desc, width, values) {
  this.name = name
  this.desc = desc
  this.type = "enum"
  this.width = width
  this.values = values
  this.format = function(obj) {
    for (const v of values) {
      if (v[1] == obj[name])
        return v[0]
    }
    return obj[name].toString() + "???"
  }
  this.get_control_value = function(html_elem) {return html_elem.value}
  this.parse = function(inp) {
      const inp_tr = inp.trim()
      for (const v of values) {
        if (inp_tr === v[0])
          return v[1]
      }
      const p = new parser(inp)
      p.skipWs()
      p.error("invalid value")
    }
}

////////////////////////////////////////////////////////////////////////////////
function make_dvs(sym, mem_bits, reg_bits) {
  if (reg_bits == undefined)
    reg_bits = mem_bits
  return [sym,
            {mem: mem_bits / 8,
             zero_fill: (reg_bits - mem_bits) / 8}]
}
function schema_datasize(name, desc, width) {
  return new schema_enum(name, desc, width,
                         [make_dvs("D8U32",  8, 32),
                          make_dvs("D16U32", 16, 32),
                          make_dvs("D32",    32),
                          make_dvs("D64",    64),
                          make_dvs("D96",    96),
                          make_dvs("D128",   128),
                        ])
}
function error_handler(check)
{
  this.errors = []
  this.check = check
  //
  this.error = function(msg) {
    if (this.check)
      throw new Error(msg)
    else {
      console.log("ERROR:", msg)
      this.errors.push(msg)
    }
  }
  this.fatal = function(msg) {
    throw new Error(msg)
  }
  this.assert_integral = function(x) {
    if (x == undefined)
      this.fatal("undefined integral param")
    if (!Number.isInteger(x))
      this.fatal("non-integral param")
    return x
  }
  this.assert_boolean = function(x) {
    if (x == undefined)
      this.fatal("undefined boolean param")
    if (typeof(x) != "boolean") {
      this.fatal("non-boolean param")
    }
    return x
  }
}

function params(schema) {
  // various fields set by parse_params() later
  this.schema = schema
  this.str = function() {
    let s = ""
    this.schema.fields.forEach((e,ix) => {
      if (ix > 0)
        s += ","
      s += e.name + ":" + e.format(this)
    })
  }
}

function schema(name, fields, validate, render, examples, fail_tests) {
  if (typeof(name) != "string")
    throw "schema.name should be a string"
  this.name = name
  //
  // sanity check fields
  for (const f_ix in fields) {
    const f = fields[f_ix]
    function check_field(f, field, type) {
      if (!f[field] || typeof(f[field]) != type) {
        throw Error("schema_field." + field + " should be " + type)
      }
    }
    function check_int_field(f, field) {
      if (!f[field] || !Number.isInteger(f[field])) {
        throw Error("schema_field." + field + " should be an integer")
      }
    }
    check_field(f, "name", "string")
    check_field(f, "desc", "string")
    check_field(f, "type", "string")
    check_int_field(f, "width")
    check_field(f, "format", "function")
    check_field(f, "get_control_value", "function")
    check_field(f, "parse", "function")
  }
  this.fields = fields
  //
  this.parse_params = function(inps) {
      let i = 0
      let obj = new params(this)
      for (let f of this.fields) {
        const inp = inps[i++]
        obj[f.name] = f.parse(inp)
      }
      this.validate(obj)
      return obj
    }
  //
  if (validate && typeof(validate) != "function")
    throw "schema.validate should be a function"
  this.validate = validate
  //
  if (render && typeof(render) != "function")
    throw "schema.render should be a function"
  this.render = render
  this.examples = examples
  this.fail_tests = fail_tests
}


////////////////////////////////////////////////////////////////////////////////
function get_elem_or_die(e_id) {
  const e = document.getElementById(e_id)
  if (e == null)
    throw Error(e_id + ": cannot find element")
  return e
}

////////////////////////////////////////////////////////////////////////////////
let next_glb_id = 0

// TODO: small X in top right to remove panel from parent
function add_panel(sc)
{
  const main = get_elem_or_die("main-panel")

  const our_id = next_glb_id++
  const e = sc.examples[our_id % sc.examples.length]
  // console.log(e)
  if (e.length != sc.fields.length) {
    console.log(e)
    throw Error("example[" + (our_id % sc.examples.length) + "] in schema table wrong number of inputs")
  }

  let new_input = "<hr />\n"
  for (let f_ix = 0; f_ix < sc.fields.length; f_ix++) {
    const f = sc.fields[f_ix]
    if (f.type == "intseq" && f_ix > 0) {
      new_input += "<br/>\n"
    }
    // TODO: datasize gets a dropdown
    // TODO: set input names from f.get_id(our_id)
    const maybe_width = f.width ? ";width:" + f.width + "px" : ""
    const inp_id = schema_get_field_id(f.name, our_id)
    const init_val = f.parse(e[f_ix])
    let x =
      // "<span style=\"border:black 1px solid\">\n" +
      "<span class=\"nts\">" + f.desc + ": </span>"
    if (f.type == "boolean") {
      const maybe_checked = init_val ? " checked" : ""
      x +=
        "<input type=\"checkbox\"" +
        " id=\"" + inp_id + "\"" +
        " style=\"font-family:monospace" + maybe_width + "\"" + maybe_checked + "></input>\n" +
        ""
    } else if (f.type == "enum") {
      x +=
        "<input type=\"text\"" +
        " id=\"" + inp_id + "\"" +
        " value=\"" + e[f_ix] + "\"" +
        " list=\"" + inp_id + "-values\"" +
        " style=\"font-family:monospace" + maybe_width + "\"></input>\n"
      x += "<datalist id=\"" + inp_id + "-values\">\n"
      for (const v of f.values) {
        x += "<option>" + v[0] + "</option>\n"
      }
      x += "</datalist>"
    } else {
      x +=
        "<input type=\"text\"" +
        " id=\"" + inp_id + "\"" +
        " value=\"" + e[f_ix] + "\"" +
        " style=\"font-family:monospace" + maybe_width + "\"></input>\n" +
        ""
    }
    // x += "</span>"
    new_input += x
  }
  // canned controls
  new_input +=
      "<br />\n" +
      "<input type=\"checkbox\" id=\"check-" + our_id + "\" value=\"checks\" style=\"font-family:monospace\" checked>Checks</input>\n" +
      "<input type=\"button\" id=\"button-draw-" + our_id + "\" value=\"Draw SVG\"/>\n" +
      "<input type=\"button\" id=\"button-gensrc-" + our_id + "\" value=\"Emit SVG Source\"/>\n" +
      "<div id=\"img-output-" + our_id + "\">\n" +
      "Press \"Draw SVG\" to generate output " + our_id + "\n" +
      "</div>\n" +
      "<pre id=\"svg-output-" + our_id + "\"></pre>\n"

  const new_div = document.createElement("div")
  new_div.innerHTML = new_input
  new_div.style.border = "1px solid black"
  main.appendChild(new_div)

  // all the elements are instantiated

  function addClickHandler(pfx, bool_arg) {
    const button = get_elem_or_die(pfx + "-" + our_id)
    button.addEventListener("click", function() {
      generate_svg(sc, our_id, bool_arg)
    })
  }
  addClickHandler("button-draw", false)
  addClickHandler("button-gensrc", true)
  /*
  for (let f_ix = 0; f_ix < sc.fields.length; f_ix++) {
    const inp = get_elem_or_die("input-" + sc.fields[f_ix].name + "-" + our_id)
    let nxt_inp = null
    if (f_ix < sc.fields.length - 1) {
      nxt_inp = get_elem_or_die("input-" + sc.fields[f_ix + 1].name + "-" + our_id)
    } else {
      nxt_inp = get_elem_or_die("button-draw-" + our_id)
    }
    let prv_inp = null
    if (f_ix > 0) {
      prv_inp = get_elem_or_die("input-" + sc.fields[f_ix - 1].name + "-" + our_id)
    }
    inp.addEventListener("keydown", function(e) {
      const enter_key = 13
      const tab_key = 9
      if (e.key === "Tab") {
        e.shiftKey
        e.preventDefault();
        nxt_inp.focus()
        nxt_inp.select()
      } else {
        console.log(e)
      }
    });
  }
  */

  // TODO: tab key to next field (enter key to draw svg?)
  //       iterate fields and add enter listeners to the next
  /*
  const inp = document.getElementById("input" + our_id)
  const arg = "" + our_id;
  inp.addEventListener("keyup", function(e) {
    const enter_key = 13
    if (e.keyCode === enter_key) {
      e.preventDefault();
      writeSvgTo(arg, false)
    }
  });
  */
} // add_panel

function add_examples(sc)
{
  const main_panel = get_elem_or_die("main-panel")
  const new_div = document.createElement("div")
  new_div.innerHTML =
    `<div><p>
    Integer vector inputs typically parse a special sequence syntax (e.g. for src0 per-lane values).
    Such as sequence can be a javascript integer list literal, a repeated constant,
    or an "aseq", which generates arithmetic sequence given a base and delta.
    These subsequence terms may be concatenated with the + symbol.
    The javascript literal list may also have a repeat count (^</span><span class="nt">rep</span>).
    If shorter than the list's length, it truncates.  If longer than the list, then it cycles.
    Formally,<br/>
      <span class="nt">seq</span> =&gt; <span class="nt">seq_term</span> (<span class="te">+</span> <span class="nt">seq_term</span>)* <br/>
      <span class="nt">seq_term</span> =&gt; <br/>
          &nbsp;&nbsp;<span class="nt">JavascriptListLiteral</span> | <br/>
          &nbsp;&nbsp;<span class="nt">JavascriptListLiteral</span>^</span><span class="nt">rep</span> | <br/>
          &nbsp;&nbsp;<span class="te">aseq(</span><span class="nt">base</span><span class="te">,</span><span class="nt">off</span><span class="te">)</span><span class="nt">^</span><span class="nt">rep</span> | <br/>
          &nbsp;&nbsp;<span class="nt">const</span><span class="te">^</span><span class="nt">rep</span><br/>
        where <span class="nt">base</span>, <span class="nt">off</span>, and <span class="nt">rep</span> are signed integers (hex or decimal)
        <br/><br/>
      Seq Examples:<br/>
      <table>
      <tr><td><span class="te">[1,2,3,4]</span></td><td>generates [1,2,3,4]</td></tr>
      <tr><td><span class="te">2^4</span></td><td>generates [2,2,2,2] (^ repetition)</td></tr>
      <tr><td><span class="te">[0,6,4]^5</span></td><td>generates [0,6,4,0,6] (^ cycles smaller lists)</td></tr>
      <tr><td><span class="te">[0,6,4]^2</span></td><td>generates [0,6] (^ truncates longer lists)</td></tr>
      <tr><td><span class="te">aseq(0,4)^4</span></td><td>generates [0,4,8,12] (0 + 4 * i)</td></tr>
      <tr><td><span class="te">aseq(8,-4)^3+aseq(0,4)^3</span></td><td>generates [8,4,0,  0,4,8]</td></tr>
      <tr><td><span class="te">aseq(0,4)^4+3^4+[5,6]</span></td><td>generates [0,4,8,12,  3,3,3,3,  5,6] (extra spaces to show terms)</td></tr>
      </table>
    </p></div>`
  new_div.style.border = "1px solid black"
  main_panel.appendChild(new_div)

  console.log("schema: ", sc)
  for (const e in sc.examples) {
    add_panel(sc)
  }
} // add_examples

////////////////////////////////////////////////////////////////////////////////
function generate_svg(sc, our_id, show_src) {
  let inps = []
  for (let f_ix = 0; f_ix < sc.fields.length; f_ix++) {
    const f = sc.fields[f_ix]
    const inp_elem = get_elem_or_die("input-" + f.name + "-" + our_id)
    inps.push(f.get_control_value(inp_elem))
  }

  const checkbox = get_elem_or_die("check-" + our_id)
  const img_output = get_elem_or_die("img-output-" + our_id)
  const svg_output = get_elem_or_die("svg-output-" + our_id)

  img_output.innerHTML = ""
  svg_output.innerText = ""

  function with_phase(phase, f) {
    try {
      f()
    } catch (e) {
      if (e instanceof Error) {
        svg_output.innerHTML = "(During " + phase + ")<br />\n" +
          "<pre style=\"color:red;font-family:monospace\">" + e.message + "</pre>"
        console.log(e.stack)
      } else {
        console.log(e)
        svg_output.innerHTML = "" + e
      }
    }
  }

  let ps = null
  with_phase("Parameter Parsing", function() {
    ps = sc.parse_params(inps)
  })

  let svg_obj = null
  let svg_src = "?";
  if (ps != null) {
    const eh = new error_handler(checkbox.checked)
    with_phase("Render", function() {
      svg_obj = sc.render(our_id, ps, eh)
      svg_src = svg_obj.src
    })
  }

  if (show_src) {
    svg_output.innerText = svg_src
  } else {
    img_output.innerHTML = svg_src
    if (svg_obj != null && svg_obj.bind_listeners != null) {
      svg_obj.bind_listeners()
    }
  }
} // generate_svg
