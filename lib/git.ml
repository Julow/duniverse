open Stdune
open Sexplib.Conv

module Ref = struct
  type ref_kind = Branch | Tag [@@deriving sexp]
  type t = ref_kind * string [@@deriving sexp]

  let equal_ref_kind a b =
    match a, b with
    | Branch, Branch | Tag, Tag -> true
    | _ -> false

  let pp_ref_kind fmt = function
    | Branch -> Format.pp_print_string fmt "Branch"
    | Tag -> Format.pp_print_string fmt "Tag"

  let equal (ak, an) (bk, bn) =
    equal_ref_kind ak bk && String.equal an bn

  let pp fmt (kind, name) =
    let open Pp_combinators.Ocaml in
    Format.fprintf fmt "@[<hov 2>(%a, %S)@]" pp_ref_kind kind name

  type resolved = { kind : ref_kind; name : string; commit : string } [@@deriving sexp]

  let ref_name ref =
    match ref.kind with
    | Branch -> "heads/" ^ ref.name
    | Tag -> "tags/" ^ ref.name

  let equal_resolved r r' =
    equal_ref_kind r.kind r'.kind
    && String.equal r.name r'.name
    && String.equal r.commit r'.commit

  let pp_resolved fmt resolved =
    Format.fprintf fmt "@[<hov 2>{ kind = %a;@ name = %s;@ commit = %s }@]"
      pp_ref_kind resolved.kind
      resolved.name
      resolved.commit
end

module Ls_remote = struct
  let non_packed_suffix = "^{}"

  let ref_arg (_, ref_name) = Bos.Cmd.(v ref_name % (ref_name ^ non_packed_suffix))

  let parse_output_line s =
    match String.extract_blank_separated_words s with
    | [ commit; ref ] -> Ok (commit, ref)
    | _ -> Error (`Msg (Printf.sprintf "Invalid git ls-remote output line: %S" s))

  type search_result = { maybe_packed : string option; not_packed : string option }

  let interpret_search_result sr =
    match sr with
    | { maybe_packed = None; not_packed = None } -> None
    | { maybe_packed = Some commit; not_packed = None }
    | { maybe_packed = _; not_packed = Some commit } ->
        Some commit

  let search_ref target lines =
    let target_not_packed = target ^ non_packed_suffix in
    let f acc (commit, full_ref) =
      if String.equal full_ref target then { acc with maybe_packed = Some commit }
      else if String.equal full_ref target_not_packed then { acc with not_packed = Some commit }
      else acc
    in
    List.fold_left ~f ~init:{ maybe_packed = None; not_packed = None } lines
    |> interpret_search_result

  let search_tag name lines =
    search_ref ("refs/tags/" ^ name) lines

  let search_branch name lines =
    search_ref ("refs/heads/" ^ name) lines

  let commit_pointed_by ~ref:(kind_opt, name) output_lines =
    let open Result.O in
    match output_lines with
    | [ "" ] -> Error `No_such_ref
    | _ -> (
        Result.List.map ~f:parse_output_line output_lines >>= fun lines ->
        let result kind = function
          | Some commit -> Ok { Ref.kind; name; commit }
          | None -> Error `No_such_ref
        in
        match kind_opt with
        | Some Ref.Tag -> result Ref.Tag (search_tag name lines)
        | Some Branch -> result Ref.Branch (search_branch name lines)
        | None ->
          match search_tag name lines, search_branch name lines with
          | Some _, Some _ -> Error `Multiple_such_refs
          | r, None -> result Ref.Tag r
          | None, r -> result Ref.Branch r )
end
