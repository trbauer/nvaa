"use strict";

function render_load(glb_id, params, eh) {
  console.log(params)

  const simt_lanes = eh.assert_integral(params.simt_lanes)
  const mem_w = eh.assert_integral(params.bufsize.w)
  const mem_h = eh.assert_integral(params.bufsize.h)
  const data_size_mem = eh.assert_integral(params.data_size.mem)
  const data_size_zero_fill = eh.assert_integral(params.data_size.zero_fill)
  const data_size_reg = data_size_mem + data_size_zero_fill
  const vec_size = 1 // fixed for now
  const exec_mask = params.exec_mask == null ?
      full_mask(params.simt_lanes) : eh.assert_integral(params.exec_mask)
  const imm_off = eh.assert_integral(params.imm_off)
  const ureg = eh.assert_integral(params.ureg)
  const maybe_scale = eh.assert_boolean(params.offset_scaling) ?
      data_size_mem * vec_size : 1

  // mem contents
  // Map's byte offset to arrays of bytes that map there
  const mem_contents = new Map()
  for (let b = 0; b < mem_h * mem_w; b++) {
    mem_contents.set(b, [])
  }
  const total_data_regs = data_size_reg * vec_size / 4
  const bytes_per_reg = simt_lanes * 4
  const reg_contents = [] // 2d array register bytes to byte() above
  for (let r = 0; r < total_data_regs; r++) {
    reg_contents[r] = []
    for (let b = 0; b < bytes_per_reg; b++) // 128 B per register
      reg_contents[r].push([])
  }

  ////////////////////////////////////////////////
  // map the bytes
  let final_addrs = []
  for (let lane = 0; lane < simt_lanes; lane++) {
    if ((exec_mask & (1 << lane)) == 0) {
      final_addrs.push(null)
      continue
    }
    // e.g. fixed start addresses can be row or column
    let last_mem_ptr = null
    const mem_start_off_vec0 =
            ureg +
              maybe_scale * params.src0_offs[lane] +
                imm_off
    final_addrs.push(mem_start_off_vec0)
    for (let v_ix = 0; v_ix < vec_size; v_ix++) {
      const mem_start_offset = mem_start_off_vec0 + v_ix * data_size_mem
      // console.log("lane" + lane + ", mem_start_offset: ", mem_start_offset)
      for (let b_ix = 0; b_ix < data_size_mem; b_ix++) {
        const mem_ptr = mem_start_offset + b_ix
        // console.log("lane" + lane + ".v" + v_ix + ".b" + b_ix + " maps to mem " + fmt_hex(mem_ptr))
        if (mem_ptr >= mem_h * mem_w)
          eh.error("lane " + lane + ": message byte out of bounds for v" + v_ix)
        const reg_byte_this_lane = v_ix * data_size_reg + b_ix
        const reg_idx = Math.floor(reg_byte_this_lane / 4)
        const reg_off = reg_byte_this_lane % 4 + lane * 4
        if (reg_idx >= reg_contents.length)
          eh.error("lane" + lane + ".v" + v_ix + ".b" + b_ix + " maps to OOB reg dst[" + reg_idx + "]")
        if (reg_off >= bytes_per_reg)
          eh.error("lane" + lane + ".v" + v_ix + ".b" + b_ix + " maps to OOB reg byte in dst[" + reg_idx + "]." + reg_off)
        if (mem_ptr < 0 || mem_ptr >= mem_h * mem_w)
          eh.error("lane" + lane + ".v" + v_ix + ".b" + b_ix + " at OOB " + mem_ptr)
        const t = get_t(reg_byte_this_lane, data_size_mem * vec_size)
        const b = new byte_mapping(lane, mem_ptr, reg_idx, reg_off, get_color(lane, t))
        const mbyte = mem_contents.get(mem_ptr)
        if (mbyte != undefined)
          mbyte.push(b)
        if (reg_idx < reg_contents.length && reg_off < reg_contents[reg_idx].length)
          reg_contents[reg_idx][reg_off].push(b)
        last_mem_ptr = mem_ptr
      } // for: data element
      for (let zf_ix = 0; zf_ix < data_size_zero_fill; zf_ix++) { // zero-padding for d8u32 and d16u32
        const reg_byte_this_lane = v_ix * data_size_reg + data_size_mem + zf_ix
        const reg_idx = Math.floor(reg_byte_this_lane / 4)
        const reg_off = reg_byte_this_lane % 4 + lane * 4
        const b = new byte_mapping(lane, last_mem_ptr, reg_idx, reg_off, color_zero_fill, true)
        if (reg_idx >= reg_contents.length)
          eh.error("lane" + lane + ".v" + v_ix + ".z" + zf_ix + " maps to OOB reg dst[" + reg_idx + "]")
        if (reg_off >= bytes_per_reg)
          eh.error("lane" + lane + ".v" + v_ix + ".z" + zf_ix + " maps to OOB reg byte in dst[" + reg_idx + "]." + reg_off)
        if (reg_idx < reg_contents.length && reg_off < reg_contents[reg_idx].length)
          reg_contents[reg_idx][reg_off].push(b)
      } // for: data element (zero fill)
    } // for: vec_size
  } // for: lane

  //////////////////////////////////////////////////////
  const svg_cfg = {
    ind: 16,
    text_h: 12,
    px_per_byte: 12,
    text_ind: 80
  }
  //
  const svg = new svg_builder(glb_id)

  ////////////////////////////////////////////////
  // render the memory
  svg.set_pen(svg_cfg.ind, svg_cfg.ind)
  render_memory(svg, svg_cfg, mem_contents, mem_w, mem_h)

  ////////////////////////////////////////////////
  // render the dst registers
  svg.add_pen_y(svg_cfg.ind)
  render_registers(svg, svg_cfg, reg_contents, bytes_per_reg)

  ////////////////////////////////////////////////
  // render various inputs (e.g. src0, immoff)
  svg.add_pen_y(2 * svg_cfg.text_h)
  //
  svg.text("exec_mask = " + fmt_hex(exec_mask), "monospace", svg_cfg.text_h)
  svg.add_pen_y(svg_cfg.text_h)
  //
  const maybe_scaled = params.offset_scaling ? (maybe_scale +" * ") : ""
  // svg.text("src0.offs = " + maybe_scaled + fmt_hex(params.src0_offs), "monospace", svg_cfg.text_h)
  const tspans_src0 = []
  tspans_src0.push(svg.tspan("src0.offs = " + maybe_scaled + "["))
  for (let lane = 0; lane < simt_lanes; lane++) {
    if (lane > 0)
      tspans_src0.push(svg.tspan(","))
    if (((1 << lane) & exec_mask) == 0)
      tspans_src0.push(svg.tspan("???", null, "lightgray"))
    else
      tspans_src0.push(svg.tspan(fmt_hex(params.src0_offs[lane])))
  }
  tspans_src0.push(svg.tspan("]"))
  svg.text_spans("monospace", svg_cfg.text_h, tspans_src0)
  svg.add_pen_y(svg_cfg.text_h)
  //
  svg.text("ureg = " + fmt_hex(params.ureg), "monospace", svg_cfg.text_h)
  svg.add_pen_y(svg_cfg.text_h)
  svg.text("immoff = " + fmt_hex(params.imm_off), "monospace", svg_cfg.text_h)
  //
  svg.add_pen_y(2 * svg_cfg.text_h)
  const tspans_fa = []
  tspans_fa.push(svg.tspan("final_addrs = ["))
  for (let lane = 0; lane < simt_lanes; lane++) {
    if (lane > 0)
      tspans_fa.push(svg.tspan(","))
    if (final_addrs[lane] == null)
      tspans_fa.push(svg.tspan("???", null, "lightgray"))
    else
      tspans_fa.push(svg.tspan(fmt_hex(params.src0_offs[lane])))
  }
  tspans_fa.push(svg.tspan("]"))
  svg.text_spans("monospace", svg_cfg.text_h, tspans_fa)

  ////////////////////////////////////////////////
  // render errors if in ignore-error mode
  if (eh.errors.length > 0) {
    svg.add_pen_y(svg_cfg.text_h)
    svg.text("(see console for errors)", "monospace", svg_cfg.text_h)
  }

  ////////////////////////////////////////////////
  // finalize the SVG
  return {
        src:svg.get_full_svg()
      , bind_listeners:function(){bind_listeners(mem_contents, reg_contents, svg.to_id)}
      }
} // render_load

const LOAD_SCHEMA = new schema(
    "load",
    //////////////
    // fields
    [
      new schema_memsize("bufsize", "BufferSize"),
      new schema_int("simt_lanes", "SIMT", 20, fmt_dec),
      new schema_execmask("exec_mask", "ExecMask", 80, fmt_hex),
      schema_datasize("data_size", "DataSize", 60),
      new schema_int("ureg", "UREG", 60, fmt_hex),
      new schema_int("imm_off", "ImmOff", 60, fmt_hex),
      new schema_boolean("offset_scaling","Scale Src0 Offsets",24),
      new schema_intseq("src0_offs", "Src0 Offsets", 500),
    ],
    ///////////////////////
    // validation function
    function(ps) {
      // TODO: fixup set default values
      // e.g. check various combinations...
      // ensure copy sizes
      check_is_int("ureg", ps.ureg)
      check_is_int("imm_off", ps.imm_off)
      check_is_bool("offset_scaling", ps.offset_scaling)
      check_is_int_array_of_length("src0_offs", ps.src0_offs, ps.simt_lanes)
    },
    ///////////////////////
    // render SVG object
    render_load,

    //////////////////////////////////
    // examples (a list of strings...)
    [
      ["64x8",fmt_dec(4),full_mask(4),"D8U32","-4","4","false","aseq(0,8)^4"],
      ["64x8",fmt_dec(4),full_mask(4),"D16U32","-4","4","false","aseq(0,8)^4"],
      ["64x16",fmt_dec(4),full_mask(4),"D32","0x0","0x0","false","aseq(0,8)^4"],
      ["64x16",fmt_dec(4),full_mask(4),"D32","0x4","-0x4","true","aseq(0,8)^4"],
      ["64x16",fmt_dec(4),full_mask(4),"D64","0x4","-0x4","true","aseq(0,8)^4"],
      ["64x16",fmt_dec(4),"0x9","D64","0x4","-0x4","true","aseq(0,8)^4"],
      ["64x16",fmt_dec(8),"0xE5","D64","0x0","0x0","false","aseq(0,64)^8"],
      ["64x16",fmt_dec(32),full_mask(32),"D32","0x0","0x0","false","aseq(0,4)^32"],
      ["64x16",fmt_dec(32),full_mask(32),"D32","0x0","0x4","false","aseq(0,8)^32"],
      ["64x16",fmt_dec(32),full_mask(32),"D64","0x0","0x0","false","aseq(0,8)^32"],
      ["64x16",fmt_dec(32),full_mask(32),"D128","0x0","0x0","false","aseq(16,16)^31+[0]"],
      // TODO: make striped examples
    ],
    []
  )
