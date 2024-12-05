"use strict";


function add_test_panels()
{
  test_schema(LOAD_SCHEMA)
  test_schema(LOAD_LOCAL_SCHEMA)
}

function test_schema(sc)
{
  const main_panel = get_elem_or_die("main-panel")
  const new_div = document.createElement("div")
  new_div.innerHTML = `<div><p>${sc.name}</p></div>`
  new_div.style.border = "1px solid black"
  main_panel.appendChild(new_div)

  if (sc != null) {
    try {
      new_div.innerHTML = "<h2>" + sc.name + "</h2>"
      test_schema_on(sc, new_div)
    } catch (e) {
      console.log(e)
      new_div.innerHTML = sc.name + ": error setting up tests: " + e.message
    }
  }
}

let test_id = 1

function test_schema_on(sc, div)
{
  console.log(sc.name)
  //
  let failed = 0, passed = 0
  function fmt_params(ps) {
    let s = "["
    let fst = true
    for (const p of ps) {
      if (fst)
        fst = false
      else
        s += ","
      s += "\"" + p + "\""
    }
    s += "]"
    return s
  } // fmt_params

  const tbl = document.createElement("table")
  tbl.style.border = "1px solid black"
  const tr_hdr = tbl.insertRow()
  const th_test = document.createElement("th")
  th_test.innerText = "Test"
  tr_hdr.appendChild(th_test)
  const th_result = document.createElement("th")
  th_result.innerText = "Result"
  tr_hdr.appendChild(th_result)
  div.appendChild(tbl)

  function add_section_header(what) {
    const tr = tbl.insertRow()
    const td_inp = tr.insertCell()
    td_inp.colspan = "2"
    td_inp.innerHTML = what
    td_inp.className = "sec_hdr"
  }

  let td_result = null
  function test_start(inps, inp_tt) {
    test_id++
    const tr = tbl.insertRow()
    const td_inp = tr.insertCell()
    const inp_pre = document.createElement("pre")
    inp_pre.innerText = fmt_params(inps)
    if (inp_tt != undefined)
      inp_pre.title = inp_tt
    td_inp.appendChild(inp_pre)
    td_result = tr.insertCell()
  }

  function test_fail(msg, ext) {
    const err_pre = document.createElement("pre")
    err_pre.innerText = msg
    err_pre.className = "err"
    if (ext != undefined)
      err_pre.title = ext
    td_result.appendChild(err_pre)
    td_result = null
    failed++
  }
  //
  function test_pass(ext) {
    const suc_span = document.createElement("span")
    suc_span.innerText = "SUCCESS"
    suc_span.className = "succ"
    if (ext != undefined)
      suc_span.title = ext
    td_result.appendChild(suc_span)
    td_result = null
    passed++
  }

  //////////////////////////////////////////////////////////////////////////////
  // positive tests
  add_section_header("Positive Tests")
  for (const inps of sc.examples) {
    test_start(inps)
    try {
      const ps = sc.parse_params(inps)
      try {
        const eh = new error_handler(true)
        sc.render(test_id, ps, eh)
        test_pass()
      } catch (e) {
        test_fail("failed render ", e.stack)
      }
    } catch (e) {
      test_fail("failed parse ", e)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // negative tests
  add_section_header("Negative Tests")
  for (const ft of sc.fail_tests) {
    //
    // <tr><td>LABEL</td><td>RESULT</td></tr>
    // console.log(ft)
    test_start(ft.inps,  "error should contain: " + ft.err.toString())
    //
    let ps = null
    try {
      ps = sc.parse_params(ft.inps)
    } catch (e) {
      console.log(ft, e)
      test_fail("failed parse ", e)
    }
    if (ps != null) {
      try {
        const eh = new error_handler(true)
        sc.render(test_id, ps, eh)
        test_fail("test did not throw exception")
      } catch (e) {
        if (e.toString().indexOf(ft.err) >= 0) {
          test_pass(e.toString())
        } else {
          console.log(sc,ft,e)
          test_fail("exception lacked expected text", e.stack)
        }
      }
    }
  } // for fail tests
}