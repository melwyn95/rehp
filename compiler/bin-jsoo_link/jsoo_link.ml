(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let expand_directories js_files =
  let dir_contents = List.map (fun file ->
    if Sys.is_directory file then
      let dir = file in
      List.map (fun one_name -> Filename.concat dir one_name) (Array.to_list (Sys.readdir dir))
    else [file]
  ) js_files in
  List.concat dir_contents

let f { Cmd_arg.output_file; source_map; resolve_sourcemap_url; js_files } =
  let with_output f =
    match output_file with
    | None -> f stdout
    | Some file -> Filename.gen_file file f
  in
  with_output (fun output ->
      Link_js.link ~output ~files:expand_directories js_files ~source_map ~resolve_sourcemap_url)

let main = Cmdliner.Term.(pure f $ Cmd_arg.options), Cmd_arg.info

let print_file_error msg name line col =
  Format.eprintf
    "\nFile \"%s\", line %d, characters %d-%d:\n"
    name
    (line + 1)
    col
    (col + 2);
  Format.eprintf "Error: %s\n\n" msg

let _ =
  Timer.init Sys.time;
  try
    Cmdliner.Term.eval
      ~catch:false
      ~argv:(Jsoo_cmdline.normalize_argv ~warn:(warn "%s") Sys.argv)
      main
  with
  (* TODO: Print this in a way that is parseable by refmterr *)
  | Errors.UserError (msg, opt_loc) ->
      (match opt_loc with
      | Some {Parse_info.name = Some name; line; col} ->
          print_file_error msg name line col
      | _ -> Format.eprintf "Error: %s\n" msg);
      exit 1
  | (Match_failure _ | Assert_failure _ | Not_found) as exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf
        "%s: You found a bug. Please report it at \
         https://github.com/ocsigen/js_of_ocaml/issues :@."
        Sys.argv.(0);
      Format.eprintf "Error: %s@." (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
  | Failure s ->
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) s;
      exit 1
  | exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
