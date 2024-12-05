////////////////////////////////////////////////////////////////////////////////
// Adapted from github.com/trbauer
"use strict";

function svg_builder(glb_id)
{
  this.glb_id = glb_id
  //
  this.pen_x = 0
  this.pen_y = 0
  //
  this.max_pen_x = 0
  this.max_pen_y = 0
  //
  this.source = ""
  //
  // NEED more portable way to do this...
  const element = document.createElement('canvas');
  this.this_is_a_hack = element.getContext('2d');
  this.compute_text_width = function(tsp) {
    let c = this.this_is_a_hack
    function set_if(c, tsp, tsp_field, ctx_field) {
      if (!(tsp_field in tsp))
        return
      if (ctx_field == undefined)
        ctx_field = tsp_field
      c[ctx_field] = tsp[tsp_field]
    }
    set_if(c, tsp, "fill")
    set_if(c, tsp, "stroke")
    set_if(c, tsp, "font-family", "fontFamily")
    set_if(c, tsp, "font-size", "fontSize")
    return c.measureText(tsp.text).width;
  }
  //
  //////////////////////////////////////////////////////////////////////////////
  this.to_id = function(id) {return "id" + glb_id + "." + id}
  //////////////////////////////////////////////////////////////////////////////
  this.set_pen_x = function(x) {this.pen_x = x}
  this.set_pen_y = function(y) {this.pen_y = y}
  this.set_pen = function(x, y) {this.set_pen_x(x); this.set_pen_y(y)}
  this.add_pen_x = function(dx) {this.pen_x += dx}
  this.add_pen_y = function(dy) {this.pen_y += dy}
  //
  //////////////////////////////////////////////////////////////////////////////
  function svg_builder_maybe_attr(a, x) {
    if (x == undefined || x == null || x == "")
       return x
    else
      return " " + a + "=\"" + x + "\""
  }
  //////////////////////////////////////////////////////////////////////////////
  // These do not move the pen (x,y)
  this.abs_rect = function(id_sfx, x_off, y_off, w, h, fill, hint) {
    this.source +=
      "<rect" +
      " id=\"" + this.to_id(id_sfx) + "\"" +
      " x=\"" + x_off + "\"" +
      " y=\"" + y_off + "\"" +
      " width=\"" + (w - 1) + "\"" +
      " height=\"" + (h - 1) + "\"" +
      svg_builder_maybe_attr("fill", fill) +
      " stroke=\"black\"" +
      ">" +
      (hint != undefined && hint != null ? "<title>" + hint + "</title>" : "") +
      "</rect>\n"
      this.max_pen_x = Math.max(this.max_pen_x, x_off + w)
      this.max_pen_y = Math.max(this.max_pen_y, y_off + h)
  }
  //
  this.abs_text = function(str, x_off, y_off, font_family, height) {
    this.source +=
      "<text" +
      " x=\"" + x_off + "\"" +
      " y=\"" + (y_off + height - 2) + "\"" +
      " style=\"font-family:" + font_family + ";font-size:" + height + "\">" + str + "</text>\n"
    const w = this.compute_text_width(this.tspan(str, null, null, font_family, height))
    console.log(str, "is", w, "px")
    this.max_pen_x = Math.max(this.max_pen_x, x_off + w)
    this.max_pen_y = Math.max(this.max_pen_y, y_off + height)
  }
  //
  //////////////////////////////////////////////////////////////////////////////
  // These move the pen in the x direction
  this.rect = function(id_sfx, w, h, fill, hint) {
    this.abs_rect(id_sfx, this.pen_x, this.pen_y, w, h, fill, hint)
    this.add_pen_x(w)
  }
  //
  this.text = function(str, family, height, w) {
    this.abs_text(str, this.pen_x, this.pen_y, family, height)
    if (w != undefined)
      this.add_pen_x(w)
  }
  // span = [{.text=?,[.fill=..],[.stroke=..],[.font-family],[.font-size]}]
  this.text_spans = function(family, height, spans) {
    this.source +=
      "<text" +
      " x=\"" + this.pen_x + "\"" +
      " y=\"" + (this.pen_y + height - 2) + "\"" +
      " style=\"font-family:" + family + ";font-size:" + height + "\">"
    let total_w = 0
    for (let i = 0; i < spans.length; i++) {
      const tsp = spans[i]
      total_w += this.compute_text_width(tsp)
      function get_if(tsp, key) {
        return (key in tsp) ? " " + key + "=\"" + tsp[key] + "\"" : ""
      }
      this.source += "<tspan"
      this.source += get_if(tsp, "fill")
      this.source += get_if(tsp, "stroke")
      this.source += get_if(tsp, "font-family")
      this.source += get_if(tsp, "font-size")
      this.source += ">"
      this.source += tsp.text
      this.source += "</tspan>"
    }
    this.source += "</text>\n"
    //
    this.max_pen_x = Math.max(this.max_pen_x, this.pen_x + total_w)
    this.max_pen_y = Math.max(this.max_pen_y, this.pen_y + height)
  }
  this.tspan = function(txt,fill,stroke,font_family,font_size) {
    let obj = {"text":txt}
    if (fill != undefined && stroke != null)
      obj["fill"] = fill
    if (stroke != undefined && stroke != null)
      obj["stroke"] = stroke
    if (font_family != undefined && font_family != null)
      obj["font-family"] = font_family
    if (font_size != undefined && font_size != null)
      obj["font-size"] = font_size
    return obj
  }
  //////////////////////////////////////////////////////////////////////////////
  // Gets the full SVG based on max sizes
  this.get_full_svg = function() {
    return "<svg " +
      "width=\"" + this.max_pen_x + "\" " +
      "height=\"" + this.max_pen_y + "\">\n" +
      this.source +
      "</svg>\n"
  }
} // svg_builder


////////////////////////////////////////////////////////////////////////////
// colors
function makeColorF(r, g, b, efrac) {
  return {start:{r:r,g:g,b:b}, end:{r:efrac*r,g:efrac*g,b:efrac*b}}
}
function makeColor(r, g, b) {
  return makeColorF(r, g, b, 0.2)
}
const COLORS = [
  makeColor(255, 0, 0), // Red
  makeColor(255, 128, 0), // Orange
  makeColor(255, 255, 0), // Yellow
  makeColor(128, 255, 0), // Lime Green
  makeColor(0, 255, 0), // Green
  makeColor(0, 255, 128), // Cyan
  makeColor(0, 128, 255), // Blue
  makeColor(128, 0, 255), // Purple
  makeColor(255, 0, 255), // Magenta
  makeColor(255, 0, 128), // Pink
  makeColor(255, 128, 128), // Light Red
  makeColor(255, 192, 128), // Light Orange
  makeColor(255, 255, 128), // Light Yellow
  makeColor(192, 255, 128), // Light Lime Green
  makeColor(128, 255, 128), // Light Green
  makeColor(128, 255, 192), // Light Cyan
  makeColor(128, 192, 255), // Light Blue
  makeColor(192, 128, 255), // Light Purple
  makeColor(255, 128, 255), // Light Magenta
  makeColor(255, 128, 192), // Light Pink
  makeColor(255, 192, 192), // Lighter Red
  makeColor(255, 224, 192), // Lighter Orange
  makeColor(255, 255, 192), // Lighter Yellow
  makeColor(224, 255, 192), // Lighter Lime Green
  makeColor(192, 255, 192), // Lighter Green
  makeColor(192, 255, 224), // Lighter Cyan
  makeColor(192, 224, 255), // Lighter Blue
  makeColor(224, 192, 255), // Lighter Purple
  makeColor(255, 192, 255), // Lighter Magenta
  makeColor(255, 192, 224), // Lighter Pink
  makeColor(255, 224, 224), // Lightest Red
  makeColor(255, 240, 224) // Lightest Orange
];

const color_no_mapping = "rgb(90,90,90)"
const color_conflict = "rgb(192,192,192)" // "rgb(255,255,255)"
const color_zero_fill = "rgb(0,0,0)"

function get_color(lane, t) {
  var scheme = COLORS[lane % COLORS.length]
  // console.log("T:" + t + ": " + makeElemLabel(elem))
  var r = Math.round(scheme.start.r*(1 - t) + scheme.end.r*t)
  var g = Math.round(scheme.start.g*(1 - t) + scheme.end.g*t)
  var b = Math.round(scheme.start.b*(1 - t) + scheme.end.b*t)
  return "rgb(" + r + "," + g + "," + b + ")"
  // return "rgb(255,0,255)"
}

// This returns [0.0..1.0] or (0.0) if there is only 1
//   get_color(..., 0.0) returns the start scheme (brightest color))
function get_t(x, n) {
  return n == 1 ? 0.0 : x / (n - 1)
}

