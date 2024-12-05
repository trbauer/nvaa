"use strict";


function render_load_local(glb_id, params, eh) {
  console.log(params)

  const simt_lanes = eh.assert_integral(params.simt_lanes)
  const mem_w = eh.assert_integral(params.simt_lanes * 4)
  const mem_h = eh.assert_integral(params.dwords_per_lane)
  const data_size_mem = eh.assert_integral(params.data_size.mem)
  const data_size_zero_fill = eh.assert_integral(params.data_size.zero_fill)
  const data_size_reg = data_size_mem + data_size_zero_fill
  const vec_size = 1
  const exec_mask = params.exec_mask == null ?
      full_mask(params.simt_lanes) : eh.assert_integral(params.exec_mask)
  const imm_off = eh.assert_integral(params.glb_imm_off)
  const scale = eh.assert_boolean(params.offset_scaling) ?
      (data_size_mem * vec_size) : 1

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
  for (let lane = 0; lane < simt_lanes; lane++) {
    if ((exec_mask & (1 << lane)) == 0) {
      continue
    }

    // E.g. fixed start addresses can be row or column
    let last_mem_ptr = null
    for (let v_ix = 0; v_ix < vec_size; v_ix++) {
      // lane-virtual offset for this vector data element
      const mem_start_offset =
          scale * params.src0_offs[lane] + imm_off + v_ix * data_size_mem
      // console.log("lane" + lane + ", mem_start_offset: ", mem_start_offset)
      for (let b_ix = 0; b_ix < data_size_mem; b_ix++) {
        // selects the DW "row"
        const lane_dw = Math.floor((mem_start_offset + b_ix) / 4)
        // jump to that row and the lane's DW + the byte offset
        const mem_ptr = bytes_per_reg * lane_dw + 4 * lane + ((mem_start_offset + b_ix) % 4)
        // console.log("lane" + lane + ".v" + v_ix + ".b" + b_ix + " maps to mem " + fmt_hex(mem_ptr))
        if (mem_ptr + b_ix >= mem_h * mem_w) {
          eh.error("lane " + lane + ": message byte out of bounds for v" + v_ix)
        }
        const reg_byte_this_lane = v_ix * data_size_mem + b_ix
        const reg_idx = Math.floor(reg_byte_this_lane / 4)
        const reg_off = reg_byte_this_lane % 4 + lane * 4
        if (reg_idx >= reg_contents.length) {
          eh.error("lane" + lane + ".v" + v_ix + ".b" + b_ix + " maps to OOB dst reg " + reg_idx)
        }
        if (reg_off >= bytes_per_reg) {
          eh.error("lane" + lane + ".v" + v_ix + ".b" + b_ix + " maps to OOB byte in dst reg")
        }
        if (mem_ptr + b_ix >= mem_h * mem_w || mem_ptr + b_ix < 0) {
          eh.error("lane" + lane + ".v" + v_ix + ".b" + b_ix + " maps to OOB address " + fmt_hex(mem_ptr + b_ix))
        }
        const t = get_t(reg_byte_this_lane,  data_size_mem * vec_size)
        const b = new byte_mapping(lane, mem_ptr, reg_idx, reg_off, get_color(lane, t))
        const mbyte = mem_contents.get(mem_ptr)
        if (mbyte != undefined)
          mbyte.push(b)
        reg_contents[reg_idx][reg_off].push(b)
        last_mem_ptr = mem_ptr
      } // for: data element
      for (let zf = 0; zf < data_size_zero_fill; zf++) { // zero-padding for d8u32 and d16u32
        // const reg_byte_this_lane = v_ix * data_size + data_size + zf
        const reg_byte_this_lane = v_ix * data_size_reg + data_size_mem + zf
        const reg_idx = Math.floor(reg_byte_this_lane / 4)
        const reg_off = reg_byte_this_lane % 4 + lane * 4
        const charge_against_byte = last_mem_ptr
        const b = new byte_mapping(lane, charge_against_byte, reg_idx, reg_off, color_zero_fill, true)
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
  svg.text("src0.offs = " + fmt_hex(params.src0_offs), "monospace", svg_cfg.text_h)
  svg.add_pen_y(svg_cfg.text_h)
  svg.text("immoff = " + fmt_hex(params.glb_imm_off), "monospace", svg_cfg.text_h)

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
      , bind_listeners:function(){bind_listeners(mem_contents, reg_contents, svg.to_id)}}
} // render_load_local

const LOAD_LOCAL_SCHEMA = new schema(
    "load_local",
    //////////////
    // fields
    [
      new schema_int("dwords_per_lane", "Memory Dwords per lane", 24, fmt_dec),
      new schema_int("simt_lanes", "SIMT", 20, fmt_dec),
      new schema_execmask("exec_mask", "ExecMask", 80, fmt_hex),
      schema_datasize("data_size", "DataSize", 64),
      new schema_int("glb_imm_off", "ImmOff", 60, fmt_hex),
      new schema_boolean("offset_scaling","Scale Src0 Offsets", 24),
      new schema_intseq("src0_offs", "Src0 Offsets", 500),
    ],
    ///////////////////////
    // validation function
    function(ps) {
      // TODO: fixup set default values
      // e.g. check various combinations...
      // ensure copy sizes
      check_is_int_array_of_length("src0_offs", ps.src0_offs, ps.simt_lanes)
    },
    ///////////////////////
    // render SVG object
    render_load_local,

    //////////////////////////////////
    // examples (a list of strings...)
    [
      ["4",fmt_dec(32),"","D32","0x0","false","[0]+4^31"],
      ["4",fmt_dec(4),"","D32","0x0","false","[4,4,4,4]"],
      ["4",fmt_dec(32),full_mask(32),"D32","0x0","false","4^32"],
      ["4",fmt_dec(4),"","D8U32","0x0","false","[4,4,4,4]"],
      ["4",fmt_dec(4),full_mask(4),"D16U32","0x4","false","[4,8,4,4]"],
      ["4",fmt_dec(4),full_mask(4),"D32","0x4","false","[4,0,4,4]"],
      ["8",fmt_dec(4),full_mask(4),"D32","0x0","false","aseq(0,4)^4"],
      ["8",fmt_dec(4),full_mask(4),"D32","0x4","true","aseq(0,1)^4"],
      ["4",fmt_dec(4),full_mask(4),"D32","0x0","false","[5,6,7,8]"],
      ["4",fmt_dec(4),full_mask(4),"D64","0x4","false","0^4"],
      ["8",fmt_dec(4),full_mask(4),"D64","0x4","false","[0,4,8,12]"],
      ["8",fmt_dec(4),full_mask(4),"D96","0x4","false","0^4"],
      ["8",fmt_dec(4),full_mask(4),"D128","0x4","false","0^4"],
      ["8",fmt_dec(4),"0x5","D128","0x4","false","0^4"],
    ],
    [
      {err:"maps to OOB address",
       inps:["4","4","0xF","D32","-0x4","false","[0,0,0,0]"]}
    ]
  )
