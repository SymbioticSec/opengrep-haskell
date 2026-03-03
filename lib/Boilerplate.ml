(**
   Boilerplate to be used as a template when mapping the haskell CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_octal_literal (env : env) (tok : CST.octal_literal) =
  (* octal_literal *) token env tok

let map_cond_minus (env : env) (tok : CST.cond_minus) =
  (* cond_minus *) token env tok

let map_implicit_variable (env : env) (tok : CST.implicit_variable) =
  (* implicit_variable *) token env tok

let map_hex_literal (env : env) (tok : CST.hex_literal) =
  (* hex_literal *) token env tok

let map_cond_left_section_op (env : env) (tok : CST.cond_left_section_op) =
  (* cond_left_section_op *) token env tok

let map_cond_prefix_at (env : env) (tok : CST.cond_prefix_at) =
  (* cond_prefix_at *) token env tok

let map_string_ (env : env) (tok : CST.string_) =
  (* string *) token env tok

let map_cmd_layout_start_do (env : env) (tok : CST.cmd_layout_start_do) =
  (* cmd_layout_start_do *) token env tok

let map_cond_tight_bang (env : env) (tok : CST.cond_tight_bang) =
  (* cond_tight_bang *) token env tok

let map_imm_tok_bar (env : env) (tok : CST.imm_tok_bar) =
  (* "|" *) token env tok

let map_arrow (env : env) (x : CST.arrow) =
  (match x with
  | `DASHGT tok -> R.Case ("DASHGT",
      (* "->" *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\134\146" *) token env tok
    )
  )

let map_phantom_arrow (env : env) (tok : CST.phantom_arrow) =
  (* phantom_arrow *) token env tok

let map_cond_prefix_tilde (env : env) (tok : CST.cond_prefix_tilde) =
  (* cond_prefix_tilde *) token env tok

let map_do_keyword (env : env) (x : CST.do_keyword) =
  (match x with
  | `Mdo tok -> R.Case ("Mdo",
      (* "mdo" *) token env tok
    )
  | `Do tok -> R.Case ("Do",
      (* "do" *) token env tok
    )
  )

let map_cmd_layout_start_explicit (env : env) (tok : CST.cmd_layout_start_explicit) =
  (* cmd_layout_start_explicit *) token env tok

let map_tok_barrbrack (env : env) (tok : CST.tok_barrbrack) =
  (* tok_barrbrack *) token env tok

let map_variable (env : env) (tok : CST.variable) =
  (* variable *) token env tok

let map_cond_infix (env : env) (tok : CST.cond_infix) =
  (* cond_infix *) token env tok

let map_cond_prefix_dot (env : env) (tok : CST.cond_prefix_dot) =
  (* cond_prefix_dot *) token env tok

let map_pat_8b3e25a (env : env) (tok : CST.pat_8b3e25a) =
  (* pattern \) *) token env tok

let map_deriving_strategy (env : env) (x : CST.deriving_strategy) =
  (match x with
  | `Stock tok -> R.Case ("Stock",
      (* "stock" *) token env tok
    )
  | `Newt tok -> R.Case ("Newt",
      (* "newtype" *) token env tok
    )
  | `Anyc tok -> R.Case ("Anyc",
      (* "anyclass" *) token env tok
    )
  )

let map_binary_literal (env : env) (tok : CST.binary_literal) =
  (* binary_literal *) token env tok

let map_carrow (env : env) (x : CST.carrow) =
  (match x with
  | `EQGT tok -> R.Case ("EQGT",
      (* "=>" *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\135\146" *) token env tok
    )
  )

let map_forall_keyword (env : env) (x : CST.forall_keyword) =
  (match x with
  | `Forall tok -> R.Case ("Forall",
      (* "forall" *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\136\128" *) token env tok
    )
  )

let map_quasiquote_body (env : env) (tok : CST.quasiquote_body) =
  (* quasiquote_body *) token env tok

let map_cmd_texp_start (env : env) (tok : CST.cmd_texp_start) =
  (* cmd_texp_start *) token env tok

let map_cond_qual_dot (env : env) (tok : CST.cond_qual_dot) =
  (* cond_qual_dot *) token env tok

let map_cmd_brace_close (env : env) (tok : CST.cmd_brace_close) =
  (* cmd_brace_close *) token env tok

let map_type_star (env : env) (x : CST.type_star) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\152\133" *) token env tok
    )
  )

let map_cond_tight_dot (env : env) (tok : CST.cond_tight_dot) =
  (* cond_tight_dot *) token env tok

let map_cond_no_section_op (env : env) (tok : CST.cond_no_section_op) =
  (* cond_no_section_op *) token env tok

let map_imm_tok_hash (env : env) (tok : CST.imm_tok_hash) =
  (* "#" *) token env tok

let map_phantom_bar (env : env) (tok : CST.phantom_bar) =
  (* phantom_bar *) token env tok

let map_name (env : env) (tok : CST.name) =
  (* name *) token env tok

let map_cmd_layout_start_if (env : env) (tok : CST.cmd_layout_start_if) =
  (* cmd_layout_start_if *) token env tok

let map_cond_prefix_percent (env : env) (tok : CST.cond_prefix_percent) =
  (* cond_prefix_percent *) token env tok

let map_tok_barbarrbrack (env : env) (tok : CST.tok_barbarrbrack) =
  (* tok_barbarrbrack *) token env tok

let map_cond_splice (env : env) (tok : CST.cond_splice) =
  (* cond_splice *) token env tok

let map_cond_quote_start (env : env) (tok : CST.cond_quote_start) =
  (* cond_quote_start *) token env tok

let map_larrow (env : env) (x : CST.larrow) =
  (match x with
  | `LTDASH tok -> R.Case ("LTDASH",
      (* "<-" *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\134\144" *) token env tok
    )
  )

let map_cmd_brace_open (env : env) (tok : CST.cmd_brace_open) =
  (* cmd_brace_open *) token env tok

let map_cond_tight_at (env : env) (tok : CST.cond_tight_at) =
  (* cond_tight_at *) token env tok

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_cond_qualified_op (env : env) (tok : CST.cond_qualified_op) =
  (* cond_qualified_op *) token env tok

let map_cmd_layout_start_quote (env : env) (tok : CST.cmd_layout_start_quote) =
  (* cmd_layout_start_quote *) token env tok

let map_cond_layout_semicolon (env : env) (tok : CST.cond_layout_semicolon) =
  (* cond_layout_semicolon *) token env tok

let map_varsym (env : env) (tok : CST.varsym) =
  (* varsym *) token env tok

let map_namespace (env : env) (x : CST.namespace) =
  (match x with
  | `Pat tok -> R.Case ("Pat",
      (* "pattern" *) token env tok
    )
  | `Type tok -> R.Case ("Type",
      (* "type" *) token env tok
    )
  )

let map_pat_20dc0f2 (env : env) (tok : CST.pat_20dc0f2) =
  (* pattern \( *) token env tok

let map_phantom_deriving (env : env) (tok : CST.phantom_deriving) =
  (* phantom_deriving *) token env tok

let map_char (env : env) (tok : CST.char) =
  (* char *) token env tok

let map_cmd_layout_start (env : env) (tok : CST.cmd_layout_start) =
  (* cmd_layout_start *) token env tok

let map_cond_prefix_bang (env : env) (tok : CST.cond_prefix_bang) =
  (* cond_prefix_bang *) token env tok

let map_phantom_in (env : env) (tok : CST.phantom_in) =
  (* phantom_in *) token env tok

let map_cond_layout_end_explicit (env : env) (tok : CST.cond_layout_end_explicit) =
  (* cond_layout_end_explicit *) token env tok

let map_consym (env : env) (tok : CST.consym) =
  (* consym *) token env tok

let map_cond_dotdot (env : env) (tok : CST.cond_dotdot) =
  (* cond_dotdot *) token env tok

let map_fun_arrow_prec (env : env) ((v1, v2) : CST.fun_arrow_prec) =
  let v1 = (* "-" *) token env v1 in
  let v2 = (* "1" *) token env v2 in
  R.Tuple [v1; v2]

let map_cmd_layout_start_let (env : env) (tok : CST.cmd_layout_start_let) =
  (* cmd_layout_start_let *) token env tok

let map_colon2 (env : env) (x : CST.colon2) =
  (match x with
  | `COLONCOLON tok -> R.Case ("COLONCOLON",
      (* "::" *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\136\183" *) token env tok
    )
  )

let map_cond_tight_tilde (env : env) (tok : CST.cond_tight_tilde) =
  (* cond_tight_tilde *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* float *) token env tok

let map_linear_arrow (env : env) (x : CST.linear_arrow) =
  (match x with
  | `DASHGTDOT tok -> R.Case ("DASHGTDOT",
      (* "->." *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\138\184" *) token env tok
    )
  )

let map_cond_assoc_tyinst (env : env) (tok : CST.cond_assoc_tyinst) =
  (* cond_assoc_tyinst *) token env tok

let map_phantom_where (env : env) (tok : CST.phantom_where) =
  (* phantom_where *) token env tok

let map_safety (env : env) (tok : CST.safety) =
  (* safety *) token env tok

let map_label (env : env) (tok : CST.label) =
  (* label *) token env tok

let map_cond_context (env : env) (tok : CST.cond_context) =
  (* cond_context *) token env tok

let map_type_role (env : env) (x : CST.type_role) =
  (match x with
  | `Repr tok -> R.Case ("Repr",
      (* "representational" *) token env tok
    )
  | `Nomi tok -> R.Case ("Nomi",
      (* "nominal" *) token env tok
    )
  | `Phan tok -> R.Case ("Phan",
      (* "phantom" *) token env tok
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  )

let map_cond_layout_end (env : env) (tok : CST.cond_layout_end) =
  (* cond_layout_end *) token env tok

let map_cmd_texp_end (env : env) (tok : CST.cmd_texp_end) =
  (* cmd_texp_end *) token env tok

let map_cmd_layout_start_case (env : env) (tok : CST.cmd_layout_start_case) =
  (* cmd_layout_start_case *) token env tok

let map_cond_data_infix (env : env) (tok : CST.cond_data_infix) =
  (* cond_data_infix *) token env tok

let map_calling_convention (env : env) (tok : CST.calling_convention) =
  (* calling_convention *) token env tok

let map_prefix_at (env : env) ((v1, v2) : CST.prefix_at) =
  let v1 = (* cond_prefix_at *) token env v1 in
  let v2 = (* "@" *) token env v2 in
  R.Tuple [v1; v2]

let map_unboxed_bar (env : env) (x : CST.unboxed_bar) =
  (match x with
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `Imm_tok_bar x -> R.Case ("Imm_tok_bar",
      map_imm_tok_bar env x
    )
  )

let map_fun_arrow (env : env) ((v1, v2) : CST.fun_arrow) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* phantom_arrow *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_arrow env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_tok_barr_720cf3d (env : env) (x : CST.anon_choice_tok_barr_720cf3d) =
  (match x with
  | `Tok_barr x -> R.Case ("Tok_barr",
      map_tok_barrbrack env x
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\159\167" *) token env tok
    )
  )

let map_prefix_dot (env : env) ((v1, v2) : CST.prefix_dot) =
  let v1 = (* cond_prefix_dot *) token env v1 in
  let v2 = (* "." *) token env v2 in
  R.Tuple [v1; v2]

let map_bracket_open (env : env) ((v1, v2) : CST.bracket_open) =
  let v1 = (* "[" *) token env v1 in
  let v2 = (* cmd_texp_start *) token env v2 in
  R.Tuple [v1; v2]

let map_qual_dot (env : env) ((v1, v2) : CST.qual_dot) =
  let v1 = (* cond_qual_dot *) token env v1 in
  let v2 = (* "." *) token env v2 in
  R.Tuple [v1; v2]

let map_tight_dot (env : env) ((v1, v2) : CST.tight_dot) =
  let v1 = (* cond_tight_dot *) token env v1 in
  let v2 = (* "." *) token env v2 in
  R.Tuple [v1; v2]

let map_bar (env : env) ((v1, v2) : CST.bar) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* phantom_bar *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "|" *) token env v2 in
  R.Tuple [v1; v2]

let map_prefix_percent (env : env) ((v1, v2) : CST.prefix_percent) =
  let v1 = (* cond_prefix_percent *) token env v1 in
  let v2 = (* "%" *) token env v2 in
  R.Tuple [v1; v2]

let map_splice_dollars (env : env) ((v1, v2) : CST.splice_dollars) =
  let v1 = (* cond_splice *) token env v1 in
  let v2 =
    (match v2 with
    | `DOLLAR tok -> R.Case ("DOLLAR",
        (* "$" *) token env tok
      )
    | `DOLLARDOLLAR tok -> R.Case ("DOLLARDOLLAR",
        (* "$$" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let map_tight_at (env : env) ((v1, v2) : CST.tight_at) =
  let v1 = (* cond_tight_at *) token env v1 in
  let v2 = (* "@" *) token env v2 in
  R.Tuple [v1; v2]

let map_integer (env : env) (x : CST.integer) =
  (match x with
  | `Bin_lit tok -> R.Case ("Bin_lit",
      (* binary_literal *) token env tok
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Octal_lit tok -> R.Case ("Octal_lit",
      (* octal_literal *) token env tok
    )
  | `Hex_lit tok -> R.Case ("Hex_lit",
      (* hex_literal *) token env tok
    )
  )

let map_anon_choice_rep1_SEMI_eece391 (env : env) (x : CST.anon_choice_rep1_SEMI_eece391) =
  (match x with
  | `Rep1_SEMI xs -> R.Case ("Rep1_SEMI",
      R.List (List.map (token env (* ";" *)) xs)
    )
  | `Cond_layout_semi tok -> R.Case ("Cond_layout_semi",
      (* cond_layout_semicolon *) token env tok
    )
  )

let map_operator_qual_dot_head (env : env) ((v1, v2) : CST.operator_qual_dot_head) =
  let v1 = (* cond_qual_dot *) token env v1 in
  let v2 = (* varsym *) token env v2 in
  R.Tuple [v1; v2]

let map_paren_open (env : env) ((v1, v2) : CST.paren_open) =
  let v1 = map_pat_20dc0f2 env v1 in
  let v2 = (* cmd_texp_start *) token env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_cmd_layout_start_adad0e7 (env : env) (x : CST.anon_choice_cmd_layout_start_adad0e7) =
  (match x with
  | `Cmd_layout_start tok -> R.Case ("Cmd_layout_start",
      (* cmd_layout_start *) token env tok
    )
  | `Cmd_layout_start_expl tok -> R.Case ("Cmd_layout_start_expl",
      (* cmd_layout_start_explicit *) token env tok
    )
  )

let map_dotdot (env : env) ((v1, v2) : CST.dotdot) =
  let v1 = (* cond_dotdot *) token env v1 in
  let v2 = (* ".." *) token env v2 in
  R.Tuple [v1; v2]

let map_where (env : env) ((v1, v2) : CST.where) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* phantom_where *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "where" *) token env v2 in
  R.Tuple [v1; v2]

let map_layout_end (env : env) (x : CST.layout_end) =
  (match x with
  | `Cond_layout_end tok -> R.Case ("Cond_layout_end",
      (* cond_layout_end *) token env tok
    )
  | `Cond_layout_end_expl tok -> R.Case ("Cond_layout_end_expl",
      (* cond_layout_end_explicit *) token env tok
    )
  )

let map_bracket_close (env : env) ((v1, v2) : CST.bracket_close) =
  let v1 = (* "]" *) token env v1 in
  let v2 = (* cmd_texp_end *) token env v2 in
  R.Tuple [v1; v2]

let map_unboxed_close (env : env) ((v1, v2) : CST.unboxed_close) =
  let v1 = (* "#)" *) token env v1 in
  let v2 = (* cmd_texp_end *) token env v2 in
  R.Tuple [v1; v2]

let map_paren_close (env : env) ((v1, v2) : CST.paren_close) =
  let v1 = map_pat_8b3e25a env v1 in
  let v2 = (* cmd_texp_end *) token env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_cmd_layout_start_case_8ec747f (env : env) (x : CST.anon_choice_cmd_layout_start_case_8ec747f) =
  (match x with
  | `Cmd_layout_start_case tok -> R.Case ("Cmd_layout_start_case",
      (* cmd_layout_start_case *) token env tok
    )
  | `Cmd_layout_start_expl tok -> R.Case ("Cmd_layout_start_expl",
      (* cmd_layout_start_explicit *) token env tok
    )
  )

let map_stringly (env : env) (x : CST.stringly) =
  (match x with
  | `Str tok -> R.Case ("Str",
      (* string *) token env tok
    )
  | `Char tok -> R.Case ("Char",
      (* char *) token env tok
    )
  )

let map_variable_ticked (env : env) ((v1, v2, v3) : CST.variable_ticked) =
  let v1 = (* "`" *) token env v1 in
  let v2 = (* variable *) token env v2 in
  let v3 = (* "`" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_fundep (env : env) ((v1, v2, v3) : CST.fundep) =
  let v1 = R.List (List.map (token env (* variable *)) v1) in
  let v2 = map_arrow env v2 in
  let v3 = R.List (List.map (token env (* variable *)) v3) in
  R.Tuple [v1; v2; v3]

let map_any_prefix_dot (env : env) (x : CST.any_prefix_dot) =
  (match x with
  | `Qual_dot x -> R.Case ("Qual_dot",
      map_qual_dot env x
    )
  | `Prefix_dot x -> R.Case ("Prefix_dot",
      map_prefix_dot env x
    )
  )

let map_any_tight_dot (env : env) (x : CST.any_tight_dot) =
  (match x with
  | `Qual_dot x -> R.Case ("Qual_dot",
      map_qual_dot env x
    )
  | `Tight_dot x -> R.Case ("Tight_dot",
      map_tight_dot env x
    )
  )

let map_operator (env : env) (x : CST.operator) =
  (match x with
  | `Opt_cond_prefix_dot_varsym (v1, v2) -> R.Case ("Opt_cond_prefix_dot_varsym",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* cond_prefix_dot *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* varsym *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Op_hash_head (v1, v2) -> R.Case ("Op_hash_head",
      let v1 =
        (match v1 with
        | `HASH tok -> R.Case ("HASH",
            (* "#" *) token env tok
          )
        | `Imm_tok_hash x -> R.Case ("Imm_tok_hash",
            map_imm_tok_hash env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Imm_tok_hash x -> R.Case ("Imm_tok_hash",
                map_imm_tok_hash env x
              )
            | `Imm_tok_bar x -> R.Case ("Imm_tok_bar",
                map_imm_tok_bar env x
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  )

let map_type_family_injectivity (env : env) ((v1, v2, v3, v4) : CST.type_family_injectivity) =
  let v1 = map_bar env v1 in
  let v2 = (* variable *) token env v2 in
  let v3 = map_arrow env v3 in
  let v4 = R.List (List.map (token env (* variable *)) v4) in
  R.Tuple [v1; v2; v3; v4]

let map_constructor_ticked (env : env) ((v1, v2, v3) : CST.constructor_ticked) =
  let v1 = (* "`" *) token env v1 in
  let v2 = (* name *) token env v2 in
  let v3 = (* "`" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_number (env : env) (x : CST.number) =
  (match x with
  | `Int x -> R.Case ("Int",
      map_integer env x
    )
  | `Float tok -> R.Case ("Float",
      (* float *) token env tok
    )
  )

let map_unboxed_open (env : env) ((v1, v2) : CST.unboxed_open) =
  let v1 = map_paren_open env v1 in
  let v2 = map_imm_tok_hash env v2 in
  R.Tuple [v1; v2]

let map_any_prefix_bang (env : env) (x : CST.any_prefix_bang) =
  (match x with
  | `Prefix_bang (v1, v2) -> R.Case ("Prefix_bang",
      let v1 = (* cond_prefix_bang *) token env v1 in
      let v2 = (* "!" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Tight_bang (v1, v2) -> R.Case ("Tight_bang",
      let v1 = (* cond_tight_bang *) token env v1 in
      let v2 = (* "!" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_any_prefix_tilde (env : env) (x : CST.any_prefix_tilde) =
  (match x with
  | `Prefix_tilde (v1, v2) -> R.Case ("Prefix_tilde",
      let v1 = (* cond_prefix_tilde *) token env v1 in
      let v2 = (* "~" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Tight_tilde (v1, v2) -> R.Case ("Tight_tilde",
      let v1 = (* cond_tight_tilde *) token env v1 in
      let v2 = (* "~" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_abstract_family (env : env) ((v1, v2, v3) : CST.abstract_family) =
  let v1 = map_anon_choice_cmd_layout_start_adad0e7 env v1 in
  let v2 = (* ".." *) token env v2 in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

let map_plist (env : env) ((v1, v2) : CST.plist) =
  let v1 = map_bracket_open env v1 in
  let v2 = map_bracket_close env v2 in
  R.Tuple [v1; v2]

let map_prefix_tuple (env : env) ((v1, v2, v3) : CST.prefix_tuple) =
  let v1 = map_paren_open env v1 in
  let v2 = R.List (List.map (token env (* "," *)) v2) in
  let v3 = map_paren_close env v3 in
  R.Tuple [v1; v2; v3]

let map_unit_ (env : env) ((v1, v2) : CST.unit_) =
  let v1 = map_paren_open env v1 in
  let v2 = map_paren_close env v2 in
  R.Tuple [v1; v2]

let map_fundeps (env : env) ((v1, v2, v3) : CST.fundeps) =
  let v1 = map_bar env v1 in
  let v2 = map_fundep env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_fundep env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_modid_prefix (env : env) ((v1, v2) : CST.modid_prefix) =
  let v1 = (* name *) token env v1 in
  let v2 = map_any_tight_dot env v2 in
  R.Tuple [v1; v2]

let map_operator_alias (env : env) (x : CST.operator_alias) =
  map_operator env x

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Choice_str x -> R.Case ("Choice_str",
      map_stringly env x
    )
  | `Choice_int x -> R.Case ("Choice_int",
      map_number env x
    )
  )

let map_unboxed_unit (env : env) ((v1, v2) : CST.unboxed_unit) =
  let v1 = map_unboxed_open env v1 in
  let v2 = map_unboxed_close env v2 in
  R.Tuple [v1; v2]

let map_conop (env : env) (x : CST.conop) =
  (match x with
  | `Cons_op_alias tok -> R.Case ("Cons_op_alias",
      (* consym *) token env tok
    )
  | `Cons_ticked x -> R.Case ("Cons_ticked",
      map_constructor_ticked env x
    )
  )

let map_consym_prefix (env : env) ((v1, v2, v3) : CST.consym_prefix) =
  let v1 = map_paren_open env v1 in
  let v2 = (* consym *) token env v2 in
  let v3 = map_paren_close env v3 in
  R.Tuple [v1; v2; v3]

let map_qualifying_module (env : env) (xs : CST.qualifying_module) =
  R.List (List.map (map_modid_prefix env) xs)

let map_module_ (env : env) ((v1, v2) : CST.module_) =
  let v1 = R.List (List.map (map_modid_prefix env) v1) in
  let v2 = (* name *) token env v2 in
  R.Tuple [v1; v2]

let map_varop (env : env) (x : CST.varop) =
  (match x with
  | `Op x -> R.Case ("Op",
      map_operator_alias env x
    )
  | `Var_ticked x -> R.Case ("Var_ticked",
      map_variable_ticked env x
    )
  )

let map_varsym_prefix (env : env) ((v1, v2, v3) : CST.varsym_prefix) =
  let v1 = map_paren_open env v1 in
  let v2 =
    (match v2 with
    | `Op x -> R.Case ("Op",
        map_operator_alias env x
      )
    | `Op_minus tok -> R.Case ("Op_minus",
        (* "-" *) token env tok
      )
    | `Op_qual_dot_head x -> R.Case ("Op_qual_dot_head",
        map_operator_qual_dot_head env x
      )
    )
  in
  let v3 = map_paren_close env v3 in
  R.Tuple [v1; v2; v3]

let map_sym (env : env) (x : CST.sym) =
  (match x with
  | `Op_alias x -> R.Case ("Op_alias",
      map_operator_alias env x
    )
  | `Cons_op_alias tok -> R.Case ("Cons_op_alias",
      (* consym *) token env tok
    )
  )

let map_tuple_cons (env : env) (x : CST.tuple_cons) =
  (match x with
  | `Prefix_tuple x -> R.Case ("Prefix_tuple",
      map_prefix_tuple env x
    )
  | `Prefix_unbo_tuple (v1, v2, v3) -> R.Case ("Prefix_unbo_tuple",
      let v1 = map_unboxed_open env v1 in
      let v2 = R.List (List.map (token env (* "," *)) v2) in
      let v3 = map_unboxed_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prefix_unbo_sum (v1, v2, v3) -> R.Case ("Prefix_unbo_sum",
      let v1 = map_unboxed_open env v1 in
      let v2 = R.List (List.map (map_unboxed_bar env) v2) in
      let v3 = map_unboxed_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_unit_cons (env : env) (x : CST.unit_cons) =
  (match x with
  | `Unit x -> R.Case ("Unit",
      map_unit_ env x
    )
  | `Unbo_unit x -> R.Case ("Unbo_unit",
      map_unboxed_unit env x
    )
  )

let map_pconsym (env : env) (x : CST.pconsym) =
  map_consym_prefix env x

let map_qualified_arrow (env : env) ((v1, v2) : CST.qualified_arrow) =
  let v1 = map_qualifying_module env v1 in
  let v2 = map_arrow env v2 in
  R.Tuple [v1; v2]

let map_qualified_type (env : env) ((v1, v2) : CST.qualified_type) =
  let v1 = map_qualifying_module env v1 in
  let v2 = (* name *) token env v2 in
  R.Tuple [v1; v2]

let map_qualified_variable (env : env) ((v1, v2) : CST.qualified_variable) =
  let v1 = map_qualifying_module env v1 in
  let v2 = (* variable *) token env v2 in
  R.Tuple [v1; v2]

let map_qualified_varsym (env : env) ((v1, v2) : CST.qualified_varsym) =
  let v1 = map_qualifying_module env v1 in
  let v2 =
    (match v2 with
    | `Op x -> R.Case ("Op",
        map_operator_alias env x
      )
    | `Op_minus tok -> R.Case ("Op_minus",
        (* "-" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let map_qualified_constructor (env : env) ((v1, v2) : CST.qualified_constructor) =
  let v1 = map_qualifying_module env v1 in
  let v2 = (* name *) token env v2 in
  R.Tuple [v1; v2]

let map_qualified_consym (env : env) ((v1, v2) : CST.qualified_consym) =
  let v1 = map_qualifying_module env v1 in
  let v2 = (* consym *) token env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_op_minus_21bd6a6 (env : env) (x : CST.anon_choice_op_minus_21bd6a6) =
  (match x with
  | `Op_minus tok -> R.Case ("Op_minus",
      (* "-" *) token env tok
    )
  | `Choice_op x -> R.Case ("Choice_op",
      map_varop env x
    )
  | `Choice_cons_op_alias x -> R.Case ("Choice_cons_op_alias",
      map_conop env x
    )
  )

let map_pvarsym (env : env) (x : CST.pvarsym) =
  map_varsym_prefix env x

let map_con (env : env) (x : CST.con) =
  (match x with
  | `Name tok -> R.Case ("Name",
      (* name *) token env tok
    )
  | `Pcon x -> R.Case ("Pcon",
      map_pconsym env x
    )
  )

let map_do_ (env : env) (x : CST.do_) =
  (match x with
  | `Do_module (v1, v2) -> R.Case ("Do_module",
      let v1 = map_qualifying_module env v1 in
      let v2 = map_do_keyword env v2 in
      R.Tuple [v1; v2]
    )
  | `Do_kw x -> R.Case ("Do_kw",
      map_do_keyword env x
    )
  )

let map_qtyconid (env : env) (x : CST.qtyconid) =
  map_qualified_type env x

let map_qvarid (env : env) (x : CST.qvarid) =
  map_qualified_variable env x

let map_qvarsym (env : env) (x : CST.qvarsym) =
  map_qualified_varsym env x

let map_qconid (env : env) (x : CST.qconid) =
  map_qualified_constructor env x

let map_qconsym (env : env) (x : CST.qconsym) =
  map_qualified_consym env x

let map_field_names (env : env) (x : CST.field_names) =
  (match x with
  | `Field_name tok -> R.Case ("Field_name",
      (* variable *) token env tok
    )
  | `Qfield_name (v1, v2) -> R.Case ("Qfield_name",
      let v1 = map_qualifying_module env v1 in
      let v2 = (* variable *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_fixity (env : env) (x : CST.fixity) =
  (match x with
  | `Fun_arrow_fixity (v1, v2, v3) -> R.Case ("Fun_arrow_fixity",
      let v1 = (* "infixr" *) token env v1 in
      let v2 = map_fun_arrow_prec env v2 in
      let v3 = (* "->" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_infixl_opt_int_choice_op_minus_rep_COMMA_choice_op_minus (v1, v2, v3, v4) -> R.Case ("Choice_infixl_opt_int_choice_op_minus_rep_COMMA_choice_op_minus",
      let v1 =
        (match v1 with
        | `Infixl tok -> R.Case ("Infixl",
            (* "infixl" *) token env tok
          )
        | `Infixr tok -> R.Case ("Infixr",
            (* "infixr" *) token env tok
          )
        | `Infix tok -> R.Case ("Infix",
            (* "infix" *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_integer env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_anon_choice_op_minus_21bd6a6 env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_anon_choice_op_minus_21bd6a6 env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_sym_prefix (env : env) (x : CST.sym_prefix) =
  (match x with
  | `Pvar x -> R.Case ("Pvar",
      map_pvarsym env x
    )
  | `Pcon x -> R.Case ("Pcon",
      map_pconsym env x
    )
  )

let map_function_name (env : env) (x : CST.function_name) =
  (match x with
  | `Var tok -> R.Case ("Var",
      (* variable *) token env tok
    )
  | `Pvar x -> R.Case ("Pvar",
      map_pvarsym env x
    )
  )

let map_tycon (env : env) (x : CST.tycon) =
  (match x with
  | `Name tok -> R.Case ("Name",
      (* name *) token env tok
    )
  | `Pvar x -> R.Case ("Pvar",
      map_pvarsym env x
    )
  | `Tycon_arrow (v1, v2, v3) -> R.Case ("Tycon_arrow",
      let v1 = map_paren_open env v1 in
      let v2 = map_arrow env v2 in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pcon x -> R.Case ("Pcon",
      map_pconsym env x
    )
  )

let map_var (env : env) (x : CST.var) =
  (match x with
  | `Var tok -> R.Case ("Var",
      (* variable *) token env tok
    )
  | `Pvar x -> R.Case ("Pvar",
      map_pvarsym env x
    )
  )

let map_tyconids (env : env) (x : CST.tyconids) =
  (match x with
  | `Qtyc x -> R.Case ("Qtyc",
      map_qtyconid env x
    )
  | `Name tok -> R.Case ("Name",
      (* name *) token env tok
    )
  )

let map_quoter (env : env) (x : CST.quoter) =
  (match x with
  | `Qvarid x -> R.Case ("Qvarid",
      map_qvarid env x
    )
  | `Var tok -> R.Case ("Var",
      (* variable *) token env tok
    )
  )

let map_qvarsym_prefix (env : env) ((v1, v2, v3) : CST.qvarsym_prefix) =
  let v1 = map_paren_open env v1 in
  let v2 = map_qvarsym env v2 in
  let v3 = map_paren_close env v3 in
  R.Tuple [v1; v2; v3]

let map_qsym (env : env) (x : CST.qsym) =
  (match x with
  | `Qual_varsym x -> R.Case ("Qual_varsym",
      map_qvarsym env x
    )
  | `Qual_consym x -> R.Case ("Qual_consym",
      map_qconsym env x
    )
  )

let map_qconsym_prefix (env : env) ((v1, v2, v3) : CST.qconsym_prefix) =
  let v1 = map_paren_open env v1 in
  let v2 = map_qconsym env v2 in
  let v3 = map_paren_close env v3 in
  R.Tuple [v1; v2; v3]

let map_type_head_name (env : env) (x : CST.type_head_name) =
  (match x with
  | `Choice_name x -> R.Case ("Choice_name",
      map_tycon env x
    )
  | `Unit x -> R.Case ("Unit",
      map_unit_ env x
    )
  | `Plist x -> R.Case ("Plist",
      map_plist env x
    )
  )

let map_var_binding_list (env : env) ((v1, v2) : CST.var_binding_list) =
  let v1 = map_var env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_var env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_anon_choice_con_8c8f04c (env : env) (x : CST.anon_choice_con_8c8f04c) =
  (match x with
  | `Choice_name x -> R.Case ("Choice_name",
      map_con env x
    )
  | `Con_bind_list (v1, v2) -> R.Case ("Con_bind_list",
      let v1 = map_con env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_con env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

let map_tyconids_ticked (env : env) (x : CST.tyconids_ticked) =
  (match x with
  | `Tycon_ticked (v1, v2, v3) -> R.Case ("Tycon_ticked",
      let v1 = (* "`" *) token env v1 in
      let v2 = (* name *) token env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Qtycon_ticked (v1, v2, v3) -> R.Case ("Qtycon_ticked",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_qtyconid env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_varids_ticked (env : env) (x : CST.varids_ticked) =
  (match x with
  | `Var_ticked x -> R.Case ("Var_ticked",
      map_variable_ticked env x
    )
  | `Qvar_ticked (v1, v2, v3) -> R.Case ("Qvar_ticked",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_qvarid env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_pqvarsym (env : env) (x : CST.pqvarsym) =
  map_qvarsym_prefix env x

let map_conids_ticked (env : env) (x : CST.conids_ticked) =
  (match x with
  | `Cons_ticked x -> R.Case ("Cons_ticked",
      map_constructor_ticked env x
    )
  | `Qcon_ticked (v1, v2, v3) -> R.Case ("Qcon_ticked",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_qconid env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_pqconsym (env : env) (x : CST.pqconsym) =
  map_qconsym_prefix env x

let map_field_spec (env : env) (x : CST.field_spec) =
  (match x with
  | `Field_names x -> R.Case ("Field_names",
      map_field_names env x
    )
  | `Field_path (v1, v2) -> R.Case ("Field_path",
      let v1 = map_field_names env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_tight_dot env v1 in
          let v2 = (* variable *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

let map_tyconops (env : env) (x : CST.tyconops) =
  (match x with
  | `Choice_op_alias x -> R.Case ("Choice_op_alias",
      map_sym env x
    )
  | `Choice_qual_varsym x -> R.Case ("Choice_qual_varsym",
      map_qsym env x
    )
  | `Op_minus tok -> R.Case ("Op_minus",
      (* "-" *) token env tok
    )
  | `Tycons_ticked x -> R.Case ("Tycons_ticked",
      map_tyconids_ticked env x
    )
  )

let map_qvar (env : env) (x : CST.qvar) =
  (match x with
  | `Qvarid x -> R.Case ("Qvarid",
      map_qvarid env x
    )
  | `Pqva x -> R.Case ("Pqva",
      map_pqvarsym env x
    )
  )

let map_op_ticked (env : env) (x : CST.op_ticked) =
  (match x with
  | `Choice_var_ticked x -> R.Case ("Choice_var_ticked",
      map_varids_ticked env x
    )
  | `Choice_cons_ticked x -> R.Case ("Choice_cons_ticked",
      map_conids_ticked env x
    )
  )

let map_pqsym (env : env) (x : CST.pqsym) =
  (match x with
  | `Pqva x -> R.Case ("Pqva",
      map_pqvarsym env x
    )
  | `Pqco x -> R.Case ("Pqco",
      map_pqconsym env x
    )
  )

let map_qcon (env : env) (x : CST.qcon) =
  (match x with
  | `Qconid x -> R.Case ("Qconid",
      map_qconid env x
    )
  | `Pqco x -> R.Case ("Pqco",
      map_pqconsym env x
    )
  )

let map_promoted_tyconops_alias (env : env) ((v1, v2) : CST.promoted_tyconops_alias) =
  let v1 = (* "'" *) token env v1 in
  let v2 = map_tyconops env v2 in
  R.Tuple [v1; v2]

let map_vars (env : env) (x : CST.vars) =
  (match x with
  | `Choice_var x -> R.Case ("Choice_var",
      map_var env x
    )
  | `Qvar x -> R.Case ("Qvar",
      map_qvar env x
    )
  )

let map_exp_op (env : env) (x : CST.exp_op) =
  (match x with
  | `Choice_op_alias x -> R.Case ("Choice_op_alias",
      map_sym env x
    )
  | `Choice_choice_var_ticked x -> R.Case ("Choice_choice_var_ticked",
      map_op_ticked env x
    )
  | `Prefix_dot x -> R.Case ("Prefix_dot",
      map_prefix_dot env x
    )
  )

let map_ops (env : env) (x : CST.ops) =
  (match x with
  | `Op x -> R.Case ("Op",
      map_operator_alias env x
    )
  | `Qual_varsym x -> R.Case ("Qual_varsym",
      map_qvarsym env x
    )
  | `Cons_op tok -> R.Case ("Cons_op",
      (* consym *) token env tok
    )
  | `Qual_consym x -> R.Case ("Qual_consym",
      map_qconsym env x
    )
  | `Choice_choice_var_ticked x -> R.Case ("Choice_choice_var_ticked",
      map_op_ticked env x
    )
  )

let map_qtycon (env : env) (x : CST.qtycon) =
  (match x with
  | `Qtyc x -> R.Case ("Qtyc",
      map_qtyconid env x
    )
  | `Qtycon_arrow (v1, v2, v3) -> R.Case ("Qtycon_arrow",
      let v1 = map_paren_open env v1 in
      let v2 = map_qualified_arrow env v2 in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_pqva x -> R.Case ("Choice_pqva",
      map_pqsym env x
    )
  )

let map_cons (env : env) (x : CST.cons) =
  (match x with
  | `Choice_name x -> R.Case ("Choice_name",
      map_con env x
    )
  | `Choice_qconid x -> R.Case ("Choice_qconid",
      map_qcon env x
    )
  )

let map_tycons (env : env) (x : CST.tycons) =
  (match x with
  | `Choice_name x -> R.Case ("Choice_name",
      map_tycon env x
    )
  | `Choice_qtyc x -> R.Case ("Choice_qtyc",
      map_qtycon env x
    )
  )

let map_pat_name (env : env) (x : CST.pat_name) =
  (match x with
  | `Choice_var x -> R.Case ("Choice_var",
      map_var env x
    )
  | `Choice_choice_name x -> R.Case ("Choice_choice_name",
      map_cons env x
    )
  )

let map_promoted_tycons_alias (env : env) ((v1, v2) : CST.promoted_tycons_alias) =
  let v1 = (* "'" *) token env v1 in
  let v2 = map_cons env v2 in
  R.Tuple [v1; v2]

let map_exp_name (env : env) (x : CST.exp_name) =
  (match x with
  | `Choice_choice_name x -> R.Case ("Choice_choice_name",
      map_cons env x
    )
  | `Choice_choice_var x -> R.Case ("Choice_choice_var",
      map_vars env x
    )
  | `Var tok -> R.Case ("Var",
      (* variable *) token env tok
    )
  | `Impl_var tok -> R.Case ("Impl_var",
      (* implicit_variable *) token env tok
    )
  | `Label tok -> R.Case ("Label",
      (* label *) token env tok
    )
  )

let map_qname (env : env) (x : CST.qname) =
  (match x with
  | `Choice_choice_var x -> R.Case ("Choice_choice_var",
      map_vars env x
    )
  | `Choice_choice_name x -> R.Case ("Choice_choice_name",
      map_cons env x
    )
  )

let map_tyops (env : env) (x : CST.tyops) =
  (match x with
  | `Choice_choice_op_alias x -> R.Case ("Choice_choice_op_alias",
      map_tyconops env x
    )
  | `Prom_tycons (v1, v2) -> R.Case ("Prom_tycons",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_tyconops env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_child (env : env) (x : CST.child) =
  (match x with
  | `Child_type (v1, v2) -> R.Case ("Child_type",
      let v1 = (* "type" *) token env v1 in
      let v2 = map_tyconids env v2 in
      R.Tuple [v1; v2]
    )
  | `Qname x -> R.Case ("Qname",
      map_qname env x
    )
  )

let map_type_name (env : env) (x : CST.type_name) =
  (match x with
  | `Var tok -> R.Case ("Var",
      (* variable *) token env tok
    )
  | `Prom_tycons (v1, v2) -> R.Case ("Prom_tycons",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_cons env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_choice_name x -> R.Case ("Choice_choice_name",
      map_tycons env x
    )
  )

let map_anon_choice_DOTDOT_249879e (env : env) (x : CST.anon_choice_DOTDOT_249879e) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `Child x -> R.Case ("Child",
      map_child env x
    )
  )

let rec map_alternative (env : env) ((v1, v2, v3) : CST.alternative) =
  let v1 = map_pat env v1 in
  let v2 = map_matches env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_where_binds env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_alternatives (env : env) ((v1, v2, v3) : CST.alternatives) =
  let v1 =
    map_anon_choice_cmd_layout_start_case_8ec747f env v1
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_alternative env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_alternative env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_anon_rep_COMMA_opt_exp_stmt_38c9686 (env : env) (xs : CST.anon_rep_COMMA_opt_exp_stmt_38c9686) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = (* "," *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          map_exp_statement env x
        ))
      | None -> R.Option None)
    in
    R.Tuple [v1; v2]
  ) xs)

and map_assoc_datainst (env : env) (x : CST.assoc_datainst) =
  (match x with
  | `Assoc_data_adt (v1, v2, v3) -> R.Case ("Assoc_data_adt",
      let v1 = (* "data" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "instance" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_inst_adt env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Assoc_data_newt (v1, v2, v3) -> R.Case ("Assoc_data_newt",
      let v1 = (* "newtype" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "instance" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_inst_newtype env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_assoc_tyinst (env : env) ((v1, v2, v3, v4, v5) : CST.assoc_tyinst) =
  let v1 = (* "type" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "instance" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* cond_assoc_tyinst *) token env v4 in
  let v5 = map_type_instance_common env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_at_type (env : env) ((v1, v2) : CST.at_type) =
  let v1 = map_prefix_at env v1 in
  let v2 = map_newtype_con_field env v2 in
  R.Tuple [v1; v2]

and map_bind (env : env) ((v1, v2) : CST.bind) =
  let v1 =
    (match v1 with
    | `Pat x -> R.Case ("Pat",
        map_pat env x
      )
    | `Choice_var x -> R.Case ("Choice_var",
        map_var env x
      )
    | `Impl_var tok -> R.Case ("Impl_var",
        (* implicit_variable *) token env tok
      )
    )
  in
  let v2 = map_bind_matches env v2 in
  R.Tuple [v1; v2]

and map_bind_match (env : env) ((v1, v2, v3, v4) : CST.bind_match) =
  let v1 = map_guards_ env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = (* cmd_texp_end *) token env v3 in
  let v4 = map_exp_statement env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_bind_matches (env : env) ((v1, v2) : CST.bind_matches) =
  let v1 =
    (match v1 with
    | `Simple_bind_match x -> R.Case ("Simple_bind_match",
        map_simple_bind_match env x
      )
    | `Rep1_bind_match xs -> R.Case ("Rep1_bind_match",
        R.List (List.map (map_bind_match env) xs)
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_where_binds env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_class_decl (env : env) (x : CST.class_decl) =
  (match x with
  | `Local_decl x -> R.Case ("Local_decl",
      map_local_decl env x
    )
  | `Defa_sign (v1, v2) -> R.Case ("Defa_sign",
      let v1 = (* "default" *) token env v1 in
      let v2 = map_signature env v2 in
      R.Tuple [v1; v2]
    )
  | `Assoc_tyfam (v1, v2, v3, v4) -> R.Case ("Assoc_tyfam",
      let v1 = (* "type" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "family" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_type_head env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            (match x with
            | `Colon2_quan_type x -> R.Case ("Colon2_quan_type",
                map_type_annotation env x
              )
            | `Type_family_result_type_family_inje (v1, v2) -> R.Case ("Type_family_result_type_family_inje",
                let v1 = map_type_family_result env v1 in
                let v2 = map_type_family_injectivity env v2 in
                R.Tuple [v1; v2]
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Assoc_tyinst x -> R.Case ("Assoc_tyinst",
      map_assoc_tyinst env x
    )
  | `Assoc_data (v1, v2, v3) -> R.Case ("Assoc_data",
      let v1 = (* "data" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "family" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_datafam env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_class_declarations (env : env) ((v1, v2, v3) : CST.class_declarations) =
  let v1 = map_anon_choice_cmd_layout_start_adad0e7 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_class_decl env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_class_decl env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_constraint_ (env : env) (x : CST.constraint_) =
  (match x with
  | `Choice_var x -> R.Case ("Choice_var",
      map_type_name env x
    )
  | `Class_infix (v1, v2, v3, v4) -> R.Case ("Class_infix",
      let v1 = (* cond_infix *) token env v1 in
      let v2 = map_newtype_con_field env v2 in
      let v3 = map_tyops env v3 in
      let v4 = map_newtype_con_field env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Class_apply (v1, v2) -> R.Case ("Class_apply",
      let v1 = map_constraint_ env v1 in
      let v2 = map_type_apply_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Ctr_parens (v1, v2, v3) -> R.Case ("Ctr_parens",
      let v1 = map_paren_open env v1 in
      let v2 = map_constraints env v2 in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ctr_tuple (v1, v2, v3, v4) -> R.Case ("Ctr_tuple",
      let v1 = map_paren_open env v1 in
      let v2 = map_constraints env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_constraints env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_paren_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Type_wild tok -> R.Case ("Type_wild",
      (* "_" *) token env tok
    )
  | `Choice_splice x -> R.Case ("Choice_splice",
      map_universal env x
    )
  )

and map_constraints (env : env) (x : CST.constraints) =
  (match x with
  | `Cons x -> R.Case ("Cons",
      map_constraint_ env x
    )
  | `Ctr_cont (v1, v2) -> R.Case ("Ctr_cont",
      let v1 = map_context env v1 in
      let v2 = map_constraints env v2 in
      R.Tuple [v1; v2]
    )
  | `Ctr_forall (v1, v2, v3) -> R.Case ("Ctr_forall",
      let v1 = map_forall_body env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_constraints env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Impl_param x -> R.Case ("Impl_param",
      map_implicit_parameter env x
    )
  | `Ctr_sign (v1, v2, v3) -> R.Case ("Ctr_sign",
      let v1 = map_constraints env v1 in
      let v2 = map_colon2 env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_context (env : env) (x : CST.context) =
  map_context_inline env x

and map_context_inline (env : env) ((v1, v2, v3) : CST.context_inline) =
  let v1 = (* cond_context *) token env v1 in
  let v2 = map_constraint_ env v2 in
  let v3 = map_carrow env v3 in
  R.Tuple [v1; v2; v3]

and map_data (env : env) ((v1, v2, v3, v4) : CST.data) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_context env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_type_head env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_data_rhs env x
      ))
    | None -> R.Option None)
  in
  let v4 = R.List (List.map (map_deriving env) v4) in
  R.Tuple [v1; v2; v3; v4]

and map_data_constructor (env : env) ((v1, v2, v3) : CST.data_constructor) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_context env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Data_prefix x -> R.Case ("Data_prefix",
        map_datacon_prefix env x
      )
    | `Data_infix x -> R.Case ("Data_infix",
        map_datacon_infix env x
      )
    | `Data_record x -> R.Case ("Data_record",
        map_datacon_record env x
      )
    | `Data_spec x -> R.Case ("Data_spec",
        map_datacon_special env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_data_constructors (env : env) ((v1, v2) : CST.data_constructors) =
  let v1 = map_data_constructor env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_bar env v1 in
      let v2 = map_data_constructor env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_data_instance (env : env) (x : CST.data_instance) =
  (match x with
  | `Decl_inst_adt (v1, v2, v3) -> R.Case ("Decl_inst_adt",
      let v1 = (* "data" *) token env v1 in
      let v2 = (* "instance" *) token env v2 in
      let v3 = map_inst_adt env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Decl_inst_newt (v1, v2, v3) -> R.Case ("Decl_inst_newt",
      let v1 = (* "newtype" *) token env v1 in
      let v2 = (* "instance" *) token env v2 in
      let v3 = map_inst_newtype env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_data_rhs (env : env) (x : CST.data_rhs) =
  (match x with
  | `Colon2_quan_type x -> R.Case ("Colon2_quan_type",
      map_type_annotation env x
    )
  | `EQ_data_consts (v1, v2) -> R.Case ("EQ_data_consts",
      let v1 = (* "=" *) token env v1 in
      let v2 = map_data_constructors env v2 in
      R.Tuple [v1; v2]
    )
  | `Gadt x -> R.Case ("Gadt",
      map_gadt env x
    )
  )

and map_datacon_infix (env : env) ((v1, v2, v3, v4) : CST.datacon_infix) =
  let v1 = (* cond_data_infix *) token env v1 in
  let v2 = map_field_type env v2 in
  let v3 = map_conop env v3 in
  let v4 = map_field_type env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_datacon_prefix (env : env) ((v1, v2) : CST.datacon_prefix) =
  let v1 = map_con env v1 in
  let v2 = R.List (List.map (map_field_type env) v2) in
  R.Tuple [v1; v2]

and map_datacon_record (env : env) ((v1, v2) : CST.datacon_record) =
  let v1 = (* name *) token env v1 in
  let v2 = map_record_fields env v2 in
  R.Tuple [v1; v2]

and map_datacon_special (env : env) (x : CST.datacon_special) =
  (match x with
  | `Unit x -> R.Case ("Unit",
      map_unit_ env x
    )
  | `Unbo_unit x -> R.Case ("Unbo_unit",
      map_unboxed_unit env x
    )
  | `Plist x -> R.Case ("Plist",
      map_plist env x
    )
  | `Type_tuple x -> R.Case ("Type_tuple",
      map_type_tuple env x
    )
  | `Type_unbo_tuple x -> R.Case ("Type_unbo_tuple",
      map_type_unboxed_tuple env x
    )
  | `Data_unbo_sum (v1, v2, v3, v4) -> R.Case ("Data_unbo_sum",
      let v1 = map_unboxed_open env v1 in
      let v2 =
        (match v2 with
        | `Rep1_choice_BAR_quan_type (v1, v2) -> R.Case ("Rep1_choice_BAR_quan_type",
            let v1 = R.List (List.map (map_unboxed_bar env) v1) in
            let v2 = map_gadt_con_prefix env v2 in
            R.Tuple [v1; v2]
          )
        | `Quan_type_choice_BAR (v1, v2) -> R.Case ("Quan_type_choice_BAR",
            let v1 = map_gadt_con_prefix env v1 in
            let v2 = map_unboxed_bar env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v3 = R.List (List.map (map_unboxed_bar env) v3) in
      let v4 = map_unboxed_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_datafam (env : env) ((v1, v2) : CST.datafam) =
  let v1 = map_type_head env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_annotation env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_decl (env : env) (x : CST.decl) =
  (match x with
  | `Sign x -> R.Case ("Sign",
      map_signature env x
    )
  | `Func (v1, v2) -> R.Case ("Func",
      let v1 = map_function_head env v1 in
      let v2 = map_bind_matches env v2 in
      R.Tuple [v1; v2]
    )
  | `Bind x -> R.Case ("Bind",
      map_bind env x
    )
  )

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Decl x -> R.Case ("Decl",
      map_decl env x
    )
  | `Type_syno (v1, v2, v3, v4) -> R.Case ("Type_syno",
      let v1 = (* "type" *) token env v1 in
      let v2 = map_type_head env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_ktype env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Kind_sign (v1, v2, v3, v4) -> R.Case ("Kind_sign",
      let v1 = (* "type" *) token env v1 in
      let v2 = map_type_head env v2 in
      let v3 = map_colon2 env v3 in
      let v4 = map_gadt_con_prefix env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Type_family (v1, v2, v3, v4) -> R.Case ("Type_family",
      let v1 = (* "type" *) token env v1 in
      let v2 = (* "family" *) token env v2 in
      let v3 = map_tyfam env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_where env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  (match x with
                  | `Tyfam_equats x -> R.Case ("Tyfam_equats",
                      map_tyfam_equations env x
                    )
                  | `Abst_family x -> R.Case ("Abst_family",
                      map_abstract_family env x
                    )
                  )
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Type_inst_ (v1, v2, v3) -> R.Case ("Type_inst_",
      let v1 = (* "type" *) token env v1 in
      let v2 = (* "instance" *) token env v2 in
      let v3 = map_type_instance env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Role_anno (v1, v2, v3, v4) -> R.Case ("Role_anno",
      let v1 = (* "type" *) token env v1 in
      let v2 = (* "role" *) token env v2 in
      let v3 = map_tycons env v3 in
      let v4 = R.List (List.map (map_type_role env) v4) in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Data_type (v1, v2, v3) -> R.Case ("Data_type",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "type" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "data" *) token env v2 in
      let v3 = map_data env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Newt (v1, v2, v3, v4) -> R.Case ("Newt",
      let v1 = (* "newtype" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_context env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_type_head env v3 in
      let v4 = map_newtype_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Data_family (v1, v2, v3) -> R.Case ("Data_family",
      let v1 = (* "data" *) token env v1 in
      let v2 = (* "family" *) token env v2 in
      let v3 = map_datafam env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Data_inst x -> R.Case ("Data_inst",
      map_data_instance env x
    )
  | `Class (v1, v2, v3, v4, v5) -> R.Case ("Class",
      let v1 = (* "class" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_context env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_type_head env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_fundeps env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_where env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_class_declarations env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Inst (v1, v2) -> R.Case ("Inst",
      let v1 = map_instance_ env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_where env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_instance_declarations env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Defa_types (v1, v2, v3, v4) -> R.Case ("Defa_types",
      let v1 = (* "default" *) token env v1 in
      let v2 = map_paren_open env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_ktype env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_ktype env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = map_paren_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Deri_inst (v1, v2, v3, v4) -> R.Case ("Deri_inst",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* phantom_deriving *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "deriving" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            (match x with
            | `Deri_stra x -> R.Case ("Deri_stra",
                map_deriving_strategy env x
              )
            | `Via x -> R.Case ("Via",
                map_via env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v4 = map_instance_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_syno (v1, v2) -> R.Case ("Pat_syno",
      let v1 = (* "pattern" *) token env v1 in
      let v2 =
        (match v2 with
        | `Patsyn_sign x -> R.Case ("Patsyn_sign",
            map_patsyn_signature env x
          )
        | `Patsyn_equa x -> R.Case ("Patsyn_equa",
            map_patsyn_equation env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Fore_import (v1, v2, v3, v4, v5, v6) -> R.Case ("Fore_import",
      let v1 = (* "foreign" *) token env v1 in
      let v2 = (* "import" *) token env v2 in
      let v3 = (* calling_convention *) token env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* safety *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* string *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = map_signature env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Fore_export (v1, v2, v3, v4, v5) -> R.Case ("Fore_export",
      let v1 = (* "foreign" *) token env v1 in
      let v2 = (* "export" *) token env v2 in
      let v3 = (* calling_convention *) token env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* string *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_signature env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Fixity x -> R.Case ("Fixity",
      map_fixity env x
    )
  | `Top_splice x -> R.Case ("Top_splice",
      map_top_splice env x
    )
  )

and map_deriving (env : env) ((v1, v2, v3, v4, v5) : CST.deriving) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* phantom_deriving *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "deriving" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_deriving_strategy env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_constraint_ env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_via env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_exp (env : env) (x : CST.exp) =
  (match x with
  | `Exp_sign (v1, v2, v3) -> R.Case ("Exp_sign",
      let v1 = map_top_splice env v1 in
      let v2 = map_colon2 env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp x -> R.Case ("Exp",
      map_top_splice env x
    )
  )

and map_exp_greedy (env : env) (x : CST.exp_greedy) =
  (match x with
  | `Exp_lambda (v1, v2, v3, v4) -> R.Case ("Exp_lambda",
      let v1 = (* "\\" *) token env v1 in
      let v2 = map_patterns env v2 in
      let v3 = map_arrow env v3 in
      let v4 = map_exp_statement env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_let_in (v1, v2, v3, v4, v5) -> R.Case ("Exp_let_in",
      let v1 = (* "let" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_let_binds env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* phantom_in *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_exp_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Exp_cond (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Exp_cond",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_exp_statement env v2 in
      let v3 = R.List (List.map (token env (* ";" *)) v3) in
      let v4 = (* "then" *) token env v4 in
      let v5 = map_exp_statement env v5 in
      let v6 = R.List (List.map (token env (* ";" *)) v6) in
      let v7 = (* "else" *) token env v7 in
      let v8 = map_exp_statement env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  )

and map_exp_parens (env : env) ((v1, v2, v3) : CST.exp_parens) =
  let v1 = map_paren_open env v1 in
  let v2 = map_exp_statement env v2 in
  let v3 = map_paren_close env v3 in
  R.Tuple [v1; v2; v3]

and map_exp_statement (env : env) (x : CST.exp_statement) =
  map_exp env x

and map_exp_th_quoted_name (env : env) (x : CST.exp_th_quoted_name) =
  (match x with
  | `SQUOT_choice_choice_choice_var (v1, v2) -> R.Case ("SQUOT_choice_choice_choice_var",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_qname env v2 in
      R.Tuple [v1; v2]
    )
  | `SQUOTSQUOT_type (v1, v2) -> R.Case ("SQUOTSQUOT_type",
      let v1 = (* "''" *) token env v1 in
      let v2 = map_newtype_con_field env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_exp_tuple_elems (env : env) ((v1, v2) : CST.exp_tuple_elems) =
  let v1 =
    (match v1 with
    | `Rep1_COMMA_exp (v1, v2) -> R.Case ("Rep1_COMMA_exp",
        let v1 = R.List (List.map (token env (* "," *)) v1) in
        let v2 = map_exp_statement env v2 in
        R.Tuple [v1; v2]
      )
    | `Exp_COMMA_opt_exp (v1, v2, v3) -> R.Case ("Exp_COMMA_opt_exp",
        let v1 = map_exp_statement env v1 in
        let v2 = (* "," *) token env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_exp_statement env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v2 = map_anon_rep_COMMA_opt_exp_stmt_38c9686 env v2 in
  R.Tuple [v1; v2]

and map_explicit_type (env : env) ((v1, v2, v3, v4) : CST.explicit_type) =
  let v1 = map_paren_open env v1 in
  let v2 = (* "type" *) token env v2 in
  let v3 = map_newtype_con_field env v3 in
  let v4 = map_paren_close env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Exp_infix (v1, v2, v3, v4) -> R.Case ("Exp_infix",
      let v1 = map_top_splice env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* cond_no_section_op *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Cond_minus_op_minus (v1, v2) -> R.Case ("Cond_minus_op_minus",
            let v1 = (* cond_minus *) token env v1 in
            let v2 = (* "-" *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Exp_op x -> R.Case ("Exp_op",
            map_exp_op env x
          )
        | `Cond_qual_op_choice_qual_varsym (v1, v2) -> R.Case ("Cond_qual_op_choice_qual_varsym",
            let v1 = (* cond_qualified_op *) token env v1 in
            let v2 = map_qsym env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v4 = map_top_splice env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_nega (v1, v2) -> R.Case ("Exp_nega",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_top_splice env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_apply (v1, v2) -> R.Case ("Exp_apply",
      let v1 = map_top_splice env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_top_splice env x
          )
        | `At_type x -> R.Case ("At_type",
            map_at_type env x
          )
        | `Expl_type x -> R.Case ("Expl_type",
            map_explicit_type env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Exp_record (v1, v2, v3, v4, v5, v6) -> R.Case ("Exp_record",
      let v1 = map_top_splice env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = (* cmd_brace_open *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_field_update env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_field_update env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = (* "}" *) token env v5 in
      let v6 = (* cmd_brace_close *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Exp_proj (v1, v2, v3) -> R.Case ("Exp_proj",
      let v1 = map_top_splice env v1 in
      let v2 = map_tight_dot env v2 in
      let v3 = (* variable *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_arit_seq (v1, v2, v3, v4, v5, v6) -> R.Case ("Exp_arit_seq",
      let v1 = map_bracket_open env v1 in
      let v2 = map_exp_statement env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = map_exp_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = map_dotdot env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_exp_statement env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_bracket_close env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Exp_list_comp (v1, v2, v3, v4) -> R.Case ("Exp_list_comp",
      let v1 = map_bracket_open env v1 in
      let v2 = map_exp_statement env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "|" *) token env v1 in
          let v2 = map_qualifiers env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_bracket_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_unbo_tuple (v1, v2, v3, v4, v5) -> R.Case ("Exp_unbo_tuple",
      let v1 = map_unboxed_open env v1 in
      let v2 = R.List (List.map (token env (* "," *)) v2) in
      let v3 = map_exp_statement env v3 in
      let v4 = map_anon_rep_COMMA_opt_exp_stmt_38c9686 env v4 in
      let v5 = map_unboxed_close env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Exp_unbo_sum (v1, v2, v3, v4) -> R.Case ("Exp_unbo_sum",
      let v1 = map_unboxed_open env v1 in
      let v2 =
        (match v2 with
        | `Rep1_choice_BAR_exp (v1, v2) -> R.Case ("Rep1_choice_BAR_exp",
            let v1 = R.List (List.map (map_unboxed_bar env) v1) in
            let v2 = map_exp_statement env v2 in
            R.Tuple [v1; v2]
          )
        | `Exp_choice_BAR (v1, v2) -> R.Case ("Exp_choice_BAR",
            let v1 = map_exp_statement env v1 in
            let v2 = map_unboxed_bar env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v3 = R.List (List.map (map_unboxed_bar env) v3) in
      let v4 = map_unboxed_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_proj_sele (v1, v2, v3, v4, v5) -> R.Case ("Exp_proj_sele",
      let v1 = map_paren_open env v1 in
      let v2 = map_any_prefix_dot env v2 in
      let v3 = (* variable *) token env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_tight_dot env v1 in
          let v2 = (* variable *) token env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = map_paren_close env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Exp_quote (v1, v2) -> R.Case ("Exp_quote",
      let v1 =
        (match v1 with
        | `Choice_UNKUNKUNK_opt_exp (v1, v2) -> R.Case ("Choice_UNKUNKUNK_opt_exp",
            let v1 =
              (match v1 with
              | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
                  (* "\226\159\166" *) token env tok
                )
              | `Cond_quote_start_LBRACK_opt_e_BAR (v1, v2, v3, v4) -> R.Case ("Cond_quote_start_LBRACK_opt_e_BAR",
                  let v1 = (* cond_quote_start *) token env v1 in
                  let v2 = (* "[" *) token env v2 in
                  let v3 =
                    (match v3 with
                    | Some tok -> R.Option (Some (
                        (* "e" *) token env tok
                      ))
                    | None -> R.Option None)
                  in
                  let v4 = (* "|" *) token env v4 in
                  R.Tuple [v1; v2; v3; v4]
                )
              )
            in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_exp_statement env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Cond_quote_start_LBRACK_t_BAR_opt_ktype (v1, v2, v3, v4, v5) -> R.Case ("Cond_quote_start_LBRACK_t_BAR_opt_ktype",
            let v1 = (* cond_quote_start *) token env v1 in
            let v2 = (* "[" *) token env v2 in
            let v3 = (* "t" *) token env v3 in
            let v4 = (* "|" *) token env v4 in
            let v5 =
              (match v5 with
              | Some x -> R.Option (Some (
                  map_ktype env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        | `Cond_quote_start_LBRACK_p_BAR_opt_pat (v1, v2, v3, v4, v5) -> R.Case ("Cond_quote_start_LBRACK_p_BAR_opt_pat",
            let v1 = (* cond_quote_start *) token env v1 in
            let v2 = (* "[" *) token env v2 in
            let v3 = (* "p" *) token env v3 in
            let v4 = (* "|" *) token env v4 in
            let v5 =
              (match v5 with
              | Some x -> R.Option (Some (
                  map_pat env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        | `Cond_quote_start_LBRACK_d_BAR_opt_quoted_decls (v1, v2, v3, v4, v5) -> R.Case ("Cond_quote_start_LBRACK_d_BAR_opt_quoted_decls",
            let v1 = (* cond_quote_start *) token env v1 in
            let v2 = (* "[" *) token env v2 in
            let v3 = (* "d" *) token env v3 in
            let v4 = (* "|" *) token env v4 in
            let v5 =
              (match v5 with
              | Some x -> R.Option (Some (
                  map_quoted_decls env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      let v2 = map_anon_choice_tok_barr_720cf3d env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_typed_quote (v1, v2, v3, v4, v5, v6) -> R.Case ("Exp_typed_quote",
      let v1 = (* cond_quote_start *) token env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "e" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "||" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_exp_statement env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_tok_barbarrbrack env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Exp_th_quoted_name x -> R.Case ("Exp_th_quoted_name",
      map_exp_th_quoted_name env x
    )
  | `Exp_lambda_case (v1, v2, v3) -> R.Case ("Exp_lambda_case",
      let v1 = (* "\\" *) token env v1 in
      let v2 = (* "case" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_alternatives env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_lambda_cases (v1, v2, v3) -> R.Case ("Exp_lambda_cases",
      let v1 = (* "\\" *) token env v1 in
      let v2 = (* "cases" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_nalts env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_do (v1, v2) -> R.Case ("Exp_do",
      let v1 = map_do_ env v1 in
      let v2 = map_statements env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_parens x -> R.Case ("Exp_parens",
      map_exp_parens env x
    )
  | `Exp_tuple (v1, v2, v3) -> R.Case ("Exp_tuple",
      let v1 = map_paren_open env v1 in
      let v2 = map_exp_tuple_elems env v2 in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_list (v1, v2, v3, v4, v5) -> R.Case ("Exp_list",
      let v1 = map_bracket_open env v1 in
      let v2 = map_exp_statement env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_exp_statement env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_bracket_close env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Plist x -> R.Case ("Plist",
      map_plist env x
    )
  | `Exp_sect_left (v1, v2, v3, v4, v5) -> R.Case ("Exp_sect_left",
      let v1 = map_paren_open env v1 in
      let v2 = map_top_splice env v2 in
      let v3 = (* cond_left_section_op *) token env v3 in
      let v4 =
        (match v4 with
        | `Exp_op x -> R.Case ("Exp_op",
            map_exp_op env x
          )
        | `Op_minus tok -> R.Case ("Op_minus",
            (* "-" *) token env tok
          )
        | `Choice_qual_varsym x -> R.Case ("Choice_qual_varsym",
            map_qsym env x
          )
        )
      in
      let v5 = map_paren_close env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Exp_sect_right (v1, v2, v3, v4) -> R.Case ("Exp_sect_right",
      let v1 = map_paren_open env v1 in
      let v2 =
        (match v2 with
        | `Op_qual_dot_head x -> R.Case ("Op_qual_dot_head",
            map_operator_qual_dot_head env x
          )
        | `Ops x -> R.Case ("Ops",
            map_ops env x
          )
        )
      in
      let v3 = map_top_splice env v3 in
      let v4 = map_paren_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Choice_exp_lambda x -> R.Case ("Choice_exp_lambda",
      map_exp_greedy env x
    )
  | `Exp_case (v1, v2, v3, v4) -> R.Case ("Exp_case",
      let v1 = (* "case" *) token env v1 in
      let v2 = map_exp_statement env v2 in
      let v3 = (* "of" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_alternatives env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_multi_way_if (v1, v2, v3, v4) -> R.Case ("Exp_multi_way_if",
      let v1 = (* "if" *) token env v1 in
      let v2 = (* cmd_layout_start_if *) token env v2 in
      let v3 = R.List (List.map (map_match_ env) v3) in
      let v4 = (* cond_layout_end *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Choice_choice_choice_name x -> R.Case ("Choice_choice_choice_name",
      map_exp_name env x
    )
  | `Choice_splice x -> R.Case ("Choice_splice",
      map_universal env x
    )
  )

and map_field (env : env) ((v1, v2, v3, v4) : CST.field) =
  let v1 = (* variable *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* variable *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = map_colon2 env v3 in
  let v4 = map_parameter_type env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_field_pattern (env : env) (x : CST.field_pattern) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `Field_names_opt_EQ_choice_view_pat (v1, v2) -> R.Case ("Field_names_opt_EQ_choice_view_pat",
      let v1 = map_field_names env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_pat_texp env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_field_type (env : env) (x : CST.field_type) =
  (match x with
  | `Strict_field x -> R.Case ("Strict_field",
      map_strict_field env x
    )
  | `Lazy_field x -> R.Case ("Lazy_field",
      map_lazy_field env x
    )
  | `Type x -> R.Case ("Type",
      map_newtype_con_field env x
    )
  )

and map_field_update (env : env) (x : CST.field_update) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `Field_spec_opt_EQ_exp (v1, v2) -> R.Case ("Field_spec_opt_EQ_exp",
      let v1 = map_field_spec env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_simple_bind_match env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_forall_ (env : env) (x : CST.forall_) =
  (match x with
  | `Forall (v1, v2) -> R.Case ("Forall",
      let v1 = map_forall_body env v1 in
      let v2 = (* "." *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Forall_requ (v1, v2) -> R.Case ("Forall_requ",
      let v1 = map_forall_body env v1 in
      let v2 = map_arrow env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_forall_body (env : env) ((v1, v2) : CST.forall_body) =
  let v1 = map_forall_keyword env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_quantified_variables env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_function_head (env : env) (x : CST.function_head) =
  (match x with
  | `Choice_func_name_pattes (v1, v2) -> R.Case ("Choice_func_name_pattes",
      let v1 = map_function_head_patterns env v1 in
      let v2 = map_patterns env v2 in
      R.Tuple [v1; v2]
    )
  | `Func_head_infix (v1, v2, v3, v4) -> R.Case ("Func_head_infix",
      let v1 = map_pattern env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* cond_no_section_op *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Cond_minus_op_minus (v1, v2) -> R.Case ("Cond_minus_op_minus",
            let v1 = (* cond_minus *) token env v1 in
            let v2 = (* "-" *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Choice_op x -> R.Case ("Choice_op",
            map_varop env x
          )
        )
      in
      let v4 = map_pattern env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_function_head_patterns (env : env) (x : CST.function_head_patterns) =
  (match x with
  | `Func_name x -> R.Case ("Func_name",
      map_function_name env x
    )
  | `Func_head_parens (v1, v2, v3) -> R.Case ("Func_head_parens",
      let v1 = map_paren_open env v1 in
      let v2 =
        (match v2 with
        | `Choice_choice_func_name_pattes x -> R.Case ("Choice_choice_func_name_pattes",
            map_function_head env x
          )
        | `Choice_func_name x -> R.Case ("Choice_func_name",
            map_function_head_patterns env x
          )
        )
      in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_gadt (env : env) ((v1, v2, v3) : CST.gadt) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_annotation env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_where env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_gadt_constructors env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_gadt_con_prefix (env : env) (x : CST.gadt_con_prefix) =
  map_quantified_type env x

and map_gadt_con_record (env : env) ((v1, v2, v3) : CST.gadt_con_record) =
  let v1 = map_record_fields env v1 in
  let v2 = map_fun_arrow env v2 in
  let v3 = map_gadt_con_prefix env v3 in
  R.Tuple [v1; v2; v3]

and map_gadt_constructor (env : env) ((v1, v2, v3, v4, v5) : CST.gadt_constructor) =
  let v1 = map_anon_choice_con_8c8f04c env v1 in
  let v2 = map_colon2 env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_context env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | `Gadt_con_prefix x -> R.Case ("Gadt_con_prefix",
        map_gadt_con_prefix env x
      )
    | `Gadt_con_record x -> R.Case ("Gadt_con_record",
        map_gadt_con_record env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_gadt_constructors (env : env) ((v1, v2, v3) : CST.gadt_constructors) =
  let v1 = map_anon_choice_cmd_layout_start_adad0e7 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_gadt_constructor env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_gadt_constructor env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_generator (env : env) ((v1, v2, v3) : CST.generator) =
  let v1 = map_pat env v1 in
  let v2 = map_larrow env v2 in
  let v3 = map_exp_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_guard (env : env) (x : CST.guard) =
  (match x with
  | `Gene x -> R.Case ("Gene",
      map_generator env x
    )
  | `Let_ x -> R.Case ("Let_",
      map_let__ env x
    )
  | `Exp x -> R.Case ("Exp",
      map_exp_statement env x
    )
  )

and map_guards (env : env) ((v1, v2) : CST.guards) =
  let v1 = map_guard env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_guard env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_guards_ (env : env) ((v1, v2, v3) : CST.guards_) =
  let v1 = map_bar env v1 in
  let v2 = (* cmd_texp_start *) token env v2 in
  let v3 = map_guards env v3 in
  R.Tuple [v1; v2; v3]

and map_implicit_parameter (env : env) ((v1, v2, v3) : CST.implicit_parameter) =
  let v1 = (* implicit_variable *) token env v1 in
  let v2 = map_colon2 env v2 in
  let v3 = map_gadt_con_prefix env v3 in
  R.Tuple [v1; v2; v3]

and map_inferred_tyvar (env : env) ((v1, v2, v3, v4, v5) : CST.inferred_tyvar) =
  let v1 = (* "{" *) token env v1 in
  let v2 = (* cmd_brace_open *) token env v2 in
  let v3 = map_ktype_param env v3 in
  let v4 = (* "}" *) token env v4 in
  let v5 = (* cmd_brace_close *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_inst_adt (env : env) ((v1, v2, v3, v4, v5) : CST.inst_adt) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_context env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_type_instance_head env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_data_rhs env x
      ))
    | None -> R.Option None)
  in
  let v5 = R.List (List.map (map_deriving env) v5) in
  R.Tuple [v1; v2; v3; v4; v5]

and map_inst_newtype (env : env) ((v1, v2, v3, v4) : CST.inst_newtype) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_context env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_type_instance_head env v3 in
  let v4 = map_newtype_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_instance_ (env : env) ((v1, v2, v3, v4) : CST.instance_) =
  let v1 = (* "instance" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_context env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_type_instance_head env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_instance_decl (env : env) (x : CST.instance_decl) =
  (match x with
  | `Decl x -> R.Case ("Decl",
      map_decl env x
    )
  | `Assoc_data x -> R.Case ("Assoc_data",
      map_assoc_datainst env x
    )
  | `Assoc_tyinst x -> R.Case ("Assoc_tyinst",
      map_assoc_tyinst env x
    )
  )

and map_instance_declarations (env : env) ((v1, v2, v3) : CST.instance_declarations) =
  let v1 = map_anon_choice_cmd_layout_start_adad0e7 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_instance_decl env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_instance_decl env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_ktype (env : env) (x : CST.ktype) =
  (match x with
  | `Type_sign (v1, v2, v3) -> R.Case ("Type_sign",
      let v1 = map_gadt_con_prefix env v1 in
      let v2 = map_colon2 env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Quan_type x -> R.Case ("Quan_type",
      map_gadt_con_prefix env x
    )
  )

and map_ktype_param (env : env) (x : CST.ktype_param) =
  (match x with
  | `Type_param x -> R.Case ("Type_param",
      map_type_param env x
    )
  | `Type_param_anno (v1, v2, v3) -> R.Case ("Type_param_anno",
      let v1 = map_type_param env v1 in
      let v2 = map_colon2 env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_lazy_field (env : env) ((v1, v2) : CST.lazy_field) =
  let v1 = map_any_prefix_tilde env v1 in
  let v2 = map_newtype_con_field env v2 in
  R.Tuple [v1; v2]

and map_let__ (env : env) ((v1, v2) : CST.let__) =
  let v1 = (* "let" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_let_binds env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_let_binds (env : env) ((v1, v2, v3) : CST.let_binds) =
  let v1 =
    (match v1 with
    | `Cmd_layout_start_let tok -> R.Case ("Cmd_layout_start_let",
        (* cmd_layout_start_let *) token env tok
      )
    | `Cmd_layout_start_expl tok -> R.Case ("Cmd_layout_start_expl",
        (* cmd_layout_start_explicit *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_decl env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_decl env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_linear_fun_arrow (env : env) (x : CST.linear_fun_arrow) =
  (match x with
  | `Modi_fun_arrow (v1, v2) -> R.Case ("Modi_fun_arrow",
      let v1 = map_modifier env v1 in
      let v2 = map_fun_arrow env v2 in
      R.Tuple [v1; v2]
    )
  | `Opt_phan_arrow_linear_arrow (v1, v2) -> R.Case ("Opt_phan_arrow_linear_arrow",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* phantom_arrow *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_linear_arrow env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_local_binds (env : env) ((v1, v2, v3) : CST.local_binds) =
  let v1 = map_anon_choice_cmd_layout_start_adad0e7 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_local_decl env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_local_decl env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_local_decl (env : env) (x : CST.local_decl) =
  (match x with
  | `Fixity x -> R.Case ("Fixity",
      map_fixity env x
    )
  | `Decl x -> R.Case ("Decl",
      map_decl env x
    )
  )

and map_match_ (env : env) ((v1, v2, v3, v4, v5) : CST.match_) =
  let v1 = map_guards_ env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* phantom_arrow *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_arrow env v3 in
  let v4 = (* cmd_texp_end *) token env v4 in
  let v5 = map_exp_statement env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_matches (env : env) (x : CST.matches) =
  (match x with
  | `Simple_match (v1, v2) -> R.Case ("Simple_match",
      let v1 = map_arrow env v1 in
      let v2 = map_exp_statement env v2 in
      R.Tuple [v1; v2]
    )
  | `Rep1_match xs -> R.Case ("Rep1_match",
      R.List (List.map (map_match_ env) xs)
    )
  )

and map_modifier (env : env) ((v1, v2) : CST.modifier) =
  let v1 = map_prefix_percent env v1 in
  let v2 = map_newtype_con_field env v2 in
  R.Tuple [v1; v2]

and map_nalt (env : env) ((v1, v2, v3) : CST.nalt) =
  let v1 = map_patterns env v1 in
  let v2 = map_matches env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_where_binds env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_nalts (env : env) ((v1, v2, v3) : CST.nalts) =
  let v1 =
    map_anon_choice_cmd_layout_start_case_8ec747f env v1
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_nalt env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_nalt env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_newtype_ (env : env) ((v1, v2) : CST.newtype_) =
  let v1 =
    (match v1 with
    | `EQ_newt_cons (v1, v2) -> R.Case ("EQ_newt_cons",
        let v1 = (* "=" *) token env v1 in
        let v2 = map_newtype_constructor env v2 in
        R.Tuple [v1; v2]
      )
    | `Gadt x -> R.Case ("Gadt",
        map_gadt env x
      )
    )
  in
  let v2 = R.List (List.map (map_deriving env) v2) in
  R.Tuple [v1; v2]

and map_newtype_con_field (env : env) (x : CST.newtype_con_field) =
  map_type_ env x

and map_newtype_constructor (env : env) ((v1, v2) : CST.newtype_constructor) =
  let v1 = map_con env v1 in
  let v2 =
    (match v2 with
    | `Newt_con_field x -> R.Case ("Newt_con_field",
        map_newtype_con_field env x
      )
    | `Record_fields x -> R.Case ("Record_fields",
        map_record_fields env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_parameter_type (env : env) (x : CST.parameter_type) =
  (match x with
  | `Strict_field x -> R.Case ("Strict_field",
      map_strict_field env x
    )
  | `Lazy_field x -> R.Case ("Lazy_field",
      map_lazy_field env x
    )
  | `Quan_type x -> R.Case ("Quan_type",
      map_gadt_con_prefix env x
    )
  )

and map_pat (env : env) (x : CST.pat) =
  (match x with
  | `Pat_sign (v1, v2, v3) -> R.Case ("Pat_sign",
      let v1 = map_pattern env v1 in
      let v2 = map_colon2 env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  )

and map_pat_apply_arg (env : env) (x : CST.pat_apply_arg) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `At_type x -> R.Case ("At_type",
      map_at_type env x
    )
  | `Expl_type x -> R.Case ("Expl_type",
      map_explicit_type env x
    )
  )

and map_pat_texp (env : env) (x : CST.pat_texp) =
  (match x with
  | `View_pat (v1, v2, v3) -> R.Case ("View_pat",
      let v1 = map_exp_statement env v1 in
      let v2 = map_arrow env v2 in
      let v3 = map_pat_texp env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat x -> R.Case ("Pat",
      map_pat env x
    )
  )

and map_pat_tuple_elems (env : env) ((v1, v2) : CST.pat_tuple_elems) =
  let v1 = map_pat_texp env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pat_texp env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_patsyn_cons (env : env) ((v1, v2, v3) : CST.patsyn_cons) =
  let v1 = map_anon_choice_cmd_layout_start_adad0e7 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_bind env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_bind env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_patsyn_equation (env : env) ((v1, v2, v3, v4) : CST.patsyn_equation) =
  let v1 = map_pattern env v1 in
  let v2 =
    (match v2 with
    | `EQ tok -> R.Case ("EQ",
        (* "=" *) token env tok
      )
    | `Larrow x -> R.Case ("Larrow",
        map_larrow env x
      )
    )
  in
  let v3 = map_pat env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_where env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_patsyn_cons env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_patsyn_signature (env : env) ((v1, v2, v3) : CST.patsyn_signature) =
  let v1 = map_anon_choice_con_8c8f04c env v1 in
  let v2 = map_colon2 env v2 in
  let v3 = map_gadt_con_prefix env v3 in
  R.Tuple [v1; v2; v3]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Pat_infix (v1, v2, v3, v4) -> R.Case ("Pat_infix",
      let v1 = map_pattern env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* cond_no_section_op *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Cons_op tok -> R.Case ("Cons_op",
            (* consym *) token env tok
          )
        | `Choice_cons_ticked x -> R.Case ("Choice_cons_ticked",
            map_conids_ticked env x
          )
        | `Cond_qual_op_qual_consym (v1, v2) -> R.Case ("Cond_qual_op_qual_consym",
            let v1 = (* cond_qualified_op *) token env v1 in
            let v2 = map_qconsym env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v4 = map_pattern env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_nega (v1, v2) -> R.Case ("Pat_nega",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_number env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_apply (v1, v2) -> R.Case ("Pat_apply",
      let v1 = map_pattern env v1 in
      let v2 = map_pat_apply_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_choice_var x -> R.Case ("Choice_choice_var",
      map_pat_name env x
    )
  | `Pat_as (v1, v2, v3) -> R.Case ("Pat_as",
      let v1 = (* variable *) token env v1 in
      let v2 = map_tight_at env v2 in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_record (v1, v2, v3, v4, v5, v6) -> R.Case ("Pat_record",
      let v1 = map_pattern env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = (* cmd_brace_open *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_field_pattern env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_field_pattern env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = (* "}" *) token env v5 in
      let v6 = (* cmd_brace_close *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Pat_wild tok -> R.Case ("Pat_wild",
      (* "_" *) token env tok
    )
  | `Pat_parens (v1, v2, v3) -> R.Case ("Pat_parens",
      let v1 = map_paren_open env v1 in
      let v2 = map_pat_texp env v2 in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_tuple (v1, v2, v3) -> R.Case ("Pat_tuple",
      let v1 = map_paren_open env v1 in
      let v2 = map_pat_tuple_elems env v2 in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_unbo_tuple (v1, v2, v3, v4) -> R.Case ("Pat_unbo_tuple",
      let v1 = map_unboxed_open env v1 in
      let v2 = map_pat_texp env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_pat_texp env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_unboxed_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_unbo_sum (v1, v2, v3, v4) -> R.Case ("Pat_unbo_sum",
      let v1 = map_unboxed_open env v1 in
      let v2 =
        (match v2 with
        | `Rep1_choice_BAR_choice_view_pat (v1, v2) -> R.Case ("Rep1_choice_BAR_choice_view_pat",
            let v1 = R.List (List.map (map_unboxed_bar env) v1) in
            let v2 = map_pat_texp env v2 in
            R.Tuple [v1; v2]
          )
        | `Choice_view_pat_choice_BAR (v1, v2) -> R.Case ("Choice_view_pat_choice_BAR",
            let v1 = map_pat_texp env v1 in
            let v2 = map_unboxed_bar env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v3 = R.List (List.map (map_unboxed_bar env) v3) in
      let v4 = map_unboxed_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_list (v1, v2, v3, v4, v5) -> R.Case ("Pat_list",
      let v1 = map_bracket_open env v1 in
      let v2 = map_pat_texp env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_pat_texp env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_bracket_close env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Plist x -> R.Case ("Plist",
      map_plist env x
    )
  | `Pat_strict (v1, v2) -> R.Case ("Pat_strict",
      let v1 = map_any_prefix_bang env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_irre (v1, v2) -> R.Case ("Pat_irre",
      let v1 = map_any_prefix_tilde env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_splice x -> R.Case ("Choice_splice",
      map_universal env x
    )
  )

and map_patterns (env : env) (xs : CST.patterns) =
  R.List (List.map (map_pat_apply_arg env) xs)

and map_qualifier (env : env) (x : CST.qualifier) =
  (match x with
  | `Gene x -> R.Case ("Gene",
      map_generator env x
    )
  | `Let_ x -> R.Case ("Let_",
      map_let__ env x
    )
  | `Tran (v1, v2, v3) -> R.Case ("Tran",
      let v1 = (* "then" *) token env v1 in
      let v2 = map_exp_statement env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "by" *) token env v1 in
            let v2 = map_exp_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Group (v1, v2, v3, v4, v5) -> R.Case ("Group",
      let v1 = (* "then" *) token env v1 in
      let v2 = (* "group" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "by" *) token env v1 in
            let v2 = map_exp_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = (* "using" *) token env v4 in
      let v5 = map_exp_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Exp x -> R.Case ("Exp",
      map_exp_statement env x
    )
  )

and map_qualifiers (env : env) ((v1, v2) : CST.qualifiers) =
  let v1 = map_qualifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_qualifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_quantified_type (env : env) (x : CST.quantified_type) =
  (match x with
  | `Qtype_func (v1, v2, v3) -> R.Case ("Qtype_func",
      let v1 = map_parameter_type env v1 in
      let v2 = map_fun_arrow env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Qtype_linear_func (v1, v2, v3) -> R.Case ("Qtype_linear_func",
      let v1 = map_parameter_type env v1 in
      let v2 = map_linear_fun_arrow env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Qtype_forall (v1, v2, v3) -> R.Case ("Qtype_forall",
      let v1 = map_forall_body env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Qtype_forall_requ (v1, v2, v3) -> R.Case ("Qtype_forall_requ",
      let v1 = map_forall_body env v1 in
      let v2 = map_arrow env v2 in
      let v3 = map_gadt_con_prefix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Qtype_cont (v1, v2) -> R.Case ("Qtype_cont",
      let v1 = map_context env v1 in
      let v2 = map_gadt_con_prefix env v2 in
      R.Tuple [v1; v2]
    )
  | `Impl_param x -> R.Case ("Impl_param",
      map_implicit_parameter env x
    )
  | `Type x -> R.Case ("Type",
      map_newtype_con_field env x
    )
  )

and map_quantified_variables (env : env) (xs : CST.quantified_variables) =
  R.List (List.map (fun x ->
    (match x with
    | `Type_param x -> R.Case ("Type_param",
        map_type_param env x
      )
    | `Infe_tyvar x -> R.Case ("Infe_tyvar",
        map_inferred_tyvar env x
      )
    )
  ) xs)

and map_quoted_decls (env : env) ((v1, v2, v3) : CST.quoted_decls) =
  let v1 =
    (match v1 with
    | `Cmd_layout_start_quote tok -> R.Case ("Cmd_layout_start_quote",
        (* cmd_layout_start_quote *) token env tok
      )
    | `Cmd_layout_start_expl tok -> R.Case ("Cmd_layout_start_expl",
        (* cmd_layout_start_explicit *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_declaration env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_declaration env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_record_fields (env : env) ((v1, v2, v3, v4, v5, v6) : CST.record_fields) =
  let v1 = (* "{" *) token env v1 in
  let v2 = (* cmd_brace_open *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_field env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_field env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  let v6 = (* cmd_brace_close *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_signature (env : env) ((v1, v2, v3) : CST.signature) =
  let v1 =
    (match v1 with
    | `Choice_var x -> R.Case ("Choice_var",
        map_var env x
      )
    | `Var_bind_list x -> R.Case ("Var_bind_list",
        map_var_binding_list env x
      )
    )
  in
  let v2 = map_colon2 env v2 in
  let v3 = map_gadt_con_prefix env v3 in
  R.Tuple [v1; v2; v3]

and map_simple_bind_match (env : env) ((v1, v2) : CST.simple_bind_match) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_exp_statement env v2 in
  R.Tuple [v1; v2]

and map_splice_exp (env : env) (x : CST.splice_exp) =
  (match x with
  | `Choice_choice_choice_name x -> R.Case ("Choice_choice_choice_name",
      map_exp_name env x
    )
  | `Exp_parens x -> R.Case ("Exp_parens",
      map_exp_parens env x
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_exp_statement env x
    )
  | `Gene x -> R.Case ("Gene",
      map_generator env x
    )
  | `Let_ x -> R.Case ("Let_",
      map_let__ env x
    )
  | `Rec (v1, v2) -> R.Case ("Rec",
      let v1 = (* "rec" *) token env v1 in
      let v2 = map_statements env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_statements (env : env) ((v1, v2, v3) : CST.statements) =
  let v1 =
    (match v1 with
    | `Cmd_layout_start_do tok -> R.Case ("Cmd_layout_start_do",
        (* cmd_layout_start_do *) token env tok
      )
    | `Cmd_layout_start_expl tok -> R.Case ("Cmd_layout_start_expl",
        (* cmd_layout_start_explicit *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_statement env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_statement env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_strict_field (env : env) ((v1, v2) : CST.strict_field) =
  let v1 = map_any_prefix_bang env v1 in
  let v2 = map_newtype_con_field env v2 in
  R.Tuple [v1; v2]

and map_top_splice (env : env) (x : CST.top_splice) =
  map_expression env x

and map_tyfam (env : env) ((v1, v2) : CST.tyfam) =
  let v1 = map_type_head env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Colon2_quan_type x -> R.Case ("Colon2_quan_type",
            map_type_annotation env x
          )
        | `Tyfam_inj x -> R.Case ("Tyfam_inj",
            map_tyfam_inj env x
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_tyfam_equations (env : env) ((v1, v2, v3) : CST.tyfam_equations) =
  let v1 = map_anon_choice_cmd_layout_start_adad0e7 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_type_instance env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
            let v2 = map_type_instance env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_rep1_SEMI_eece391 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_layout_end env v3 in
  R.Tuple [v1; v2; v3]

and map_tyfam_inj (env : env) ((v1, v2) : CST.tyfam_inj) =
  let v1 = map_type_family_result env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_family_injectivity env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Choice_var x -> R.Case ("Choice_var",
      map_type_name env x
    )
  | `Type_star x -> R.Case ("Type_star",
      map_type_star env x
    )
  | `Type_wild tok -> R.Case ("Type_wild",
      (* "_" *) token env tok
    )
  | `Type_parens (v1, v2, v3) -> R.Case ("Type_parens",
      let v1 = map_paren_open env v1 in
      let v2 = map_ktype env v2 in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_prom (v1, v2) -> R.Case ("Type_prom",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | `Plist x -> R.Case ("Plist",
            map_plist env x
          )
        | `Type_tuple x -> R.Case ("Type_tuple",
            map_type_tuple env x
          )
        | `Type_list x -> R.Case ("Type_list",
            map_type_list env x
          )
        | `Prefix_tuple x -> R.Case ("Prefix_tuple",
            map_prefix_tuple env x
          )
        | `Unit x -> R.Case ("Unit",
            map_unit_ env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Type_list x -> R.Case ("Type_list",
      map_type_list env x
    )
  | `Plist x -> R.Case ("Plist",
      map_plist env x
    )
  | `Type_unbo_tuple x -> R.Case ("Type_unbo_tuple",
      map_type_unboxed_tuple env x
    )
  | `Type_unbo_sum (v1, v2, v3, v4) -> R.Case ("Type_unbo_sum",
      let v1 = map_unboxed_open env v1 in
      let v2 = map_ktype env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_unboxed_bar env v1 in
          let v2 = map_ktype env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_unboxed_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Type_tuple x -> R.Case ("Type_tuple",
      map_type_tuple env x
    )
  | `Type_infix x -> R.Case ("Type_infix",
      map_type_infix env x
    )
  | `Type_apply (v1, v2) -> R.Case ("Type_apply",
      let v1 = map_newtype_con_field env v1 in
      let v2 = map_type_apply_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_splice x -> R.Case ("Choice_splice",
      map_universal env x
    )
  )

and map_type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = map_colon2 env v1 in
  let v2 = map_gadt_con_prefix env v2 in
  R.Tuple [v1; v2]

and map_type_apply_arg (env : env) (x : CST.type_apply_arg) =
  (match x with
  | `Type x -> R.Case ("Type",
      map_newtype_con_field env x
    )
  | `At_type x -> R.Case ("At_type",
      map_at_type env x
    )
  )

and map_type_family_result (env : env) ((v1, v2) : CST.type_family_result) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_gadt_con_prefix env v2 in
  R.Tuple [v1; v2]

and map_type_head (env : env) (x : CST.type_head) =
  (match x with
  | `Type_head_params_opt_type_params (v1, v2) -> R.Case ("Type_head_params_opt_type_params",
      let v1 = map_type_head_params env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_params env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Type_head_infix (v1, v2, v3) -> R.Case ("Type_head_infix",
      let v1 = map_type_param env v1 in
      let v2 = map_tyconops env v2 in
      let v3 = map_type_param env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_type_head_params (env : env) (x : CST.type_head_params) =
  (match x with
  | `Type_head_name x -> R.Case ("Type_head_name",
      map_type_head_name env x
    )
  | `Type_head_parens (v1, v2, v3) -> R.Case ("Type_head_parens",
      let v1 = map_paren_open env v1 in
      let v2 =
        (match v2 with
        | `Choice_type_head_params_opt_type_params x -> R.Case ("Choice_type_head_params_opt_type_params",
            map_type_head env x
          )
        | `Type_head_params x -> R.Case ("Type_head_params",
            map_type_head_params env x
          )
        )
      in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_type_infix (env : env) ((v1, v2, v3) : CST.type_infix) =
  let v1 = map_newtype_con_field env v1 in
  let v2 = map_tyops env v2 in
  let v3 = map_newtype_con_field env v3 in
  R.Tuple [v1; v2; v3]

and map_type_instance (env : env) ((v1, v2) : CST.type_instance) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_type_instance_common env v2 in
  R.Tuple [v1; v2]

and map_type_instance_common (env : env) ((v1, v2, v3) : CST.type_instance_common) =
  let v1 = map_type_instance_head env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_gadt_con_prefix env v3 in
  R.Tuple [v1; v2; v3]

and map_type_instance_head (env : env) (x : CST.type_instance_head) =
  (match x with
  | `Type_inst_head_params_opt_type_pattes (v1, v2) -> R.Case ("Type_inst_head_params_opt_type_pattes",
      let v1 = map_type_instance_head_params env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_patterns env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Cond_infix_type_infix (v1, v2) -> R.Case ("Cond_infix_type_infix",
      let v1 = (* cond_infix *) token env v1 in
      let v2 = map_type_infix env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_type_instance_head_params (env : env) (x : CST.type_instance_head_params) =
  (match x with
  | `Choice_choice_name x -> R.Case ("Choice_choice_name",
      map_tycons env x
    )
  | `Type_inst_head_parens (v1, v2, v3, v4) -> R.Case ("Type_inst_head_parens",
      let v1 = map_paren_open env v1 in
      let v2 =
        (match v2 with
        | `Choice_type_inst_head_params_opt_type_pattes x -> R.Case ("Choice_type_inst_head_params_opt_type_pattes",
            map_type_instance_head env x
          )
        | `Type_inst_head_params x -> R.Case ("Type_inst_head_params",
            map_type_instance_head_params env x
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_paren_close env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_type_list (env : env) ((v1, v2, v3, v4) : CST.type_list) =
  let v1 = map_bracket_open env v1 in
  let v2 = map_ktype env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_ktype env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_bracket_close env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_type_param (env : env) (x : CST.type_param) =
  (match x with
  | `Type_param_wild tok -> R.Case ("Type_param_wild",
      (* "_" *) token env tok
    )
  | `Type_param_invi (v1, v2) -> R.Case ("Type_param_invi",
      let v1 = map_prefix_at env v1 in
      let v2 = map_type_param env v2 in
      R.Tuple [v1; v2]
    )
  | `Type_param_parens (v1, v2, v3) -> R.Case ("Type_param_parens",
      let v1 = map_paren_open env v1 in
      let v2 = map_ktype_param env v2 in
      let v3 = map_paren_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Var tok -> R.Case ("Var",
      (* variable *) token env tok
    )
  )

and map_type_params (env : env) (xs : CST.type_params) =
  R.List (List.map (map_type_param env) xs)

and map_type_patterns (env : env) (xs : CST.type_patterns) =
  R.List (List.map (map_type_apply_arg env) xs)

and map_type_tuple (env : env) ((v1, v2, v3) : CST.type_tuple) =
  let v1 = map_paren_open env v1 in
  let v2 = map_type_tuple_elems env v2 in
  let v3 = map_paren_close env v3 in
  R.Tuple [v1; v2; v3]

and map_type_tuple_elems (env : env) ((v1, v2) : CST.type_tuple_elems) =
  let v1 = map_ktype env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_ktype env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_type_unboxed_tuple (env : env) ((v1, v2, v3, v4) : CST.type_unboxed_tuple) =
  let v1 = map_unboxed_open env v1 in
  let v2 = map_ktype env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_ktype env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_unboxed_close env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_universal (env : env) (x : CST.universal) =
  (match x with
  | `Splice (v1, v2) -> R.Case ("Splice",
      let v1 = map_splice_dollars env v1 in
      let v2 = map_splice_exp env v2 in
      R.Tuple [v1; v2]
    )
  | `Quas (v1, v2, v3, v4, v5, v6) -> R.Case ("Quas",
      let v1 = (* cond_quote_start *) token env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_quoter env v3 in
      let v4 = (* "|" *) token env v4 in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* quasiquote_body *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = map_anon_choice_tok_barr_720cf3d env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Choice_unit x -> R.Case ("Choice_unit",
      map_unit_cons env x
    )
  | `Choice_prefix_tuple x -> R.Case ("Choice_prefix_tuple",
      map_tuple_cons env x
    )
  )

and map_via (env : env) ((v1, v2) : CST.via) =
  let v1 = (* "via" *) token env v1 in
  let v2 = map_gadt_con_prefix env v2 in
  R.Tuple [v1; v2]

and map_where_binds (env : env) ((v1, v2) : CST.where_binds) =
  let v1 = map_where env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_local_binds env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_children (env : env) ((v1, v2, v3) : CST.children) =
  let v1 = map_paren_open env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_DOTDOT_249879e env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_DOTDOT_249879e env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_paren_close env v3 in
  R.Tuple [v1; v2; v3]

let map_ie_entity (env : env) ((v1, v2, v3) : CST.ie_entity) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_namespace env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Choice_qvarid x -> R.Case ("Choice_qvarid",
        map_quoter env x
      )
    | `Choice_qtyc x -> R.Case ("Choice_qtyc",
        map_tyconids env x
      )
    | `Choice_sym_prefix x -> R.Case ("Choice_sym_prefix",
        (match x with
        | `Sym_prefix x -> R.Case ("Sym_prefix",
            map_sym_prefix env x
          )
        | `Choice_pqva x -> R.Case ("Choice_pqva",
            map_pqsym env x
          )
        )
      )
    )
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_children env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_import_list (env : env) ((v1, v2, v3, v4) : CST.import_list) =
  let v1 = map_paren_open env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_ie_entity env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_ie_entity env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_paren_close env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_ie_entity_50c17c0 (env : env) (x : CST.anon_choice_ie_entity_50c17c0) =
  (match x with
  | `Ie_entity x -> R.Case ("Ie_entity",
      map_ie_entity env x
    )
  | `Module_export (v1, v2) -> R.Case ("Module_export",
      let v1 = (* "module" *) token env v1 in
      let v2 = map_module_ env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_import (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.import) =
  let v1 = (* "import" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "qualified" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* string *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_module_ env v4 in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "qualified" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = map_module_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "hiding" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 = map_import_list env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_exports (env : env) ((v1, v2, v3, v4) : CST.exports) =
  let v1 = map_paren_open env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_ie_entity_50c17c0 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_ie_entity_50c17c0 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_paren_close env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_imports (env : env) ((v1, v2, v3) : CST.imports) =
  let v1 = map_import env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
      let v2 = map_import env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = map_anon_choice_rep1_SEMI_eece391 env v3 in
  R.Tuple [v1; v2; v3]

let map_declarations (env : env) ((v1, v2, v3) : CST.declarations) =
  let v1 = map_declaration env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_anon_choice_rep1_SEMI_eece391 env v1 in
      let v2 =
        (match v2 with
        | `Decl x -> R.Case ("Decl",
            map_declaration env x
          )
        | `Import x -> R.Case ("Import",
            map_import env x
          )
        )
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_rep1_SEMI_eece391 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_header (env : env) ((v1, v2, v3, v4) : CST.header) =
  let v1 = (* "module" *) token env v1 in
  let v2 = map_module_ env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_exports env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_where env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_body (env : env) ((v1, v2, v3, v4, v5) : CST.body) =
  let v1 = map_anon_choice_cmd_layout_start_adad0e7 env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_rep1_SEMI_eece391 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_imports env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_declarations env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_layout_end env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_haskell (env : env) ((v1, v2) : CST.haskell) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_header env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_haddock (env : env) (tok : CST.haddock) =
  (* haddock *) token env tok

let map_pragma (env : env) (tok : CST.pragma) =
  (* pragma *) token env tok

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_cpp (env : env) (tok : CST.cpp) =
  (* cpp *) token env tok

let dump_tree root =
  map_haskell () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Cpp (_loc, x) -> ("cpp", "cpp", map_cpp env x)
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Haddock (_loc, x) -> ("haddock", "haddock", map_haddock env x)
  | `Pragma (_loc, x) -> ("pragma", "pragma", map_pragma env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
