/**
 * Marshal
 * @providesModule Marshal
 */
"use strict";
var Bytes = require('Bytes.js');
var Pervasives = require('Pervasives.js');
var runtime = require('runtime.js');

let joo_global_object = global;


var runtime = joo_global_object.jsoo_runtime;
var caml_marshal_data_size = runtime["caml_marshal_data_size"];
var caml_ml_bytes_length = runtime["caml_ml_bytes_length"];
var caml_new_string = runtime["caml_new_string"];

function caml_call1(f, a0) {
  return f.length == 1 ? f(a0) : runtime["caml_call_gen"](f, [a0]);
}

var global_data = runtime["caml_get_global_data"]();
var cst_Marshal_from_bytes = caml_new_string("Marshal.from_bytes");
var cst_Marshal_from_bytes__0 = caml_new_string("Marshal.from_bytes");
var cst_Marshal_data_size = caml_new_string("Marshal.data_size");
var cst_Marshal_to_buffer_substring_out_of_bounds = caml_new_string(
  "Marshal.to_buffer: substring out of bounds"
);
var Bytes = global_data["Bytes"];
var Pervasives = global_data["Pervasives"];

function to_buffer(buff, ofs, len, v, flags) {
  if (0 <= ofs) {
    if (0 <= len) {
      if (! ((caml_ml_bytes_length(buff) - len | 0) < ofs)) {
        return runtime["caml_output_value_to_buffer"](buff, ofs, len, v, flags
        );
      }
    }
  }
  return caml_call1(
    Pervasives[1],
    cst_Marshal_to_buffer_substring_out_of_bounds
  );
}

var header_size = 20;

function data_size(buff, ofs) {
  if (0 <= ofs) {
    if (! ((caml_ml_bytes_length(buff) - 20 | 0) < ofs)) {return caml_marshal_data_size(buff, ofs);}
  }
  return caml_call1(Pervasives[1], cst_Marshal_data_size);
}

function total_size(buff, ofs) {return 20 + data_size(buff, ofs) | 0;}

function from_bytes(buff, ofs) {
  if (0 <= ofs) {
    if (! ((caml_ml_bytes_length(buff) - 20 | 0) < ofs)) {
      var len = caml_marshal_data_size(buff, ofs);
      return (caml_ml_bytes_length(buff) - (20 + len | 0) | 0) < ofs ?
        caml_call1(Pervasives[1], cst_Marshal_from_bytes__0) :
        runtime["caml_input_value_from_string"](buff, ofs);
    }
  }
  return caml_call1(Pervasives[1], cst_Marshal_from_bytes);
}

function from_string(buff, ofs) {
  return from_bytes(caml_call1(Bytes[43], buff), ofs);
}

function cP(cT) {return runtime["caml_input_value"](cT);}

var Marshal = [
  0,
  function(cS, cR, cQ) {return runtime["caml_output_value"](cS, cR, cQ);},
  to_buffer,
  cP,
  from_bytes,
  from_string,
  header_size,
  data_size,
  total_size
];

runtime["caml_register_global"](6, Marshal, "Marshal");


module.exports = global.jsoo_runtime.caml_get_global_data().Marshal;