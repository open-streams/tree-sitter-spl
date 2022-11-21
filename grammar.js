module.exports = grammar({
  name: 'SPL',

  conflicts: $ => [
    [$.expr, $.type],
    [$.ext, $.type],
    [$.ext, $.comp_type]
  ],

  rules: {
    source_file: $ => seq(optional($.ns), optional($.uses), $.defs),

    //
    // Top-level.
    //

    ns: $ => seq('namespace', $.ns_name, ';'),
    ns_name: $ => seq($.id, repeat(seq('.', $.id))),

    uses: $ => repeat1($.use),
    use: $ => seq('use', $.ns_name, '::', choice('*', $.id), ';'),

    defs: $ => repeat1($.def),
    def: $ => choice(
      $.comp_def,
      // $.function_def,
      $.standalone_typedef,
    ),

    //
    // config.
    //

    config: $ => seq($.id, ':', $.expr, ';'),

    //
    // comp.
    //

    comp_def: $ => seq($.comp_head, $.comp_body),
    comp_head: $ => seq(optional('public'), 'composite', $.id, optional($.comp_inout)),

    comp_inout: $ => seq(choice('input', 'output'), seq($.comp_inout_decl, repeat(seq(',', $.comp_inout_decl)))),
    comp_inout_decl: $ => seq(optional($.stream_type), $.id),

    comp_body: $ => seq('{',
      optional(seq('config', repeat1($.config))),
      optional(seq('graph', repeat1($.op_invoke))),
      optional(seq('param', repeat1($.comp_formal))),
      // seq('type', repeat1($.comp_type)),
      '}'),

    comp_formal: $ => seq($.expr_mode, $.id, optional(seq(':', $.op_actual)), ';'),

    //
    // Operator invocation.
    //

    op_invoke: $ => seq($.op_head, $.op_body),
    op_head: $ => seq($.op_outs, optional(seq('as', $.id)), '=', $.id, $.op_ins),

    op_outs: $ => choice($.op_out, seq('(', optional(seq($.op_out, repeat(seq(';', $.op_out)))), ')')),
    op_out: $ => prec.left(1, seq($.stream_type, $.id, optional(seq('as', $.id)))),

    op_ins: $ => seq('(', optional(seq($.op_in, repeat(seq(';', $.op_in)))), ')'),
    op_in: $ => seq(optional($.stream_type), seq($.id, repeat(seq(',', $.id))), optional(seq('as', $.id))),

    op_body: $ => seq('{',
      repeat(choice(
        $.op_body_configs,
        $.op_body_logic,
        $.op_body_outs,
        $.op_body_actuals,
      )), '}'),

    op_body_configs: $ => seq('config', seq($.op_body_config, repeat(seq(';', $.op_body_config)), ';')),
    op_body_config: $ => seq($.id, ':', seq($.expr, repeat(seq(',', $.expr)))),

    op_body_logic: $ => seq('logic', choice($.op_body_code, $.op_body_state)),
    op_body_code: $ => choice(seq('onProcess', ':', $.stmt), seq(choice('onTuple', 'onPunct'), $.id, ':', $.stmt),),
    op_body_state: $ => seq('state', ':', choice($.vardef, seq('{', repeat1($.vardef), '}'))),

    op_body_outs: $ => seq('output', seq($.op_body_out, repeat(seq(';', $.op_body_out))), ';'),
    op_body_out: $ => seq($.id, ':', seq($.op_out_assign, repeat(seq(',', $.op_out_assign)))),
    op_out_assign: $ => seq($.id, '=', $.expr),

    op_body_actuals: $ => seq('param', seq($.op_body_actual, repeat(seq(';', $.op_body_actual))), ';'),
    op_body_actual: $ => seq($.id, ':', $.op_actual),
    op_actual: $ => choice($.type, seq($.expr, repeat(seq(',', $.expr)))),

    //
    // type.
    //

    type: $ => choice(
      'void',
      $.primitive_type,
      $.comp_type,
      $.id,
    ),

    type_args: $ => seq('<', repeat1($.type), '>'),
    type_dims: $ => seq('[', $.expr, ']'),
    type_name: $ => prec.left(1, seq(optional(seq($.ns_name, '::')), $.id, optional(seq('.', $.id)))),

    primitive_type: $ => choice(
      'boolean',
      seq('enum', '{', $.id, repeat(seq(',', $.id)), '}'),
      'int8', 'int16', 'int32', 'int64', 'int128',
      'uint8', 'uint16', 'uint32', 'uint64', 'uint128',
      'float32', 'float64',
      'decimal32', 'decimal64', 'decimal128',
      'complex32', 'complex64',
      'timestamp',
      'blob',
      'rstring',
      'ustring',
      'xml'
    ),

    comp_type: $ => choice(
      $.tuple_type,
      seq('list', $.type_args, optional($.type_dims)),
      seq('map', $.type_args, optional($.type_dims)),
      seq('set', $.type_args, optional($.type_dims)),
      seq('optional', $.type_args),
    ),

    stream_type: $ => seq('stream', '<', $.tuple_body, '>'),

    tuple_type: $ => seq('tuple', '<', $.tuple_body, '>'),
    tuple_body: $ => choice($.attrs, $.exts),

    //
    // typedef.
    //

    standalone_typedef: $ => seq('type', $.id, '=', choice($.type, $.tuple_body), ';'),

    expr_mode: $ => choice(
      'attribute',
      seq('expression', optional($.type_args)),
      'function',
      'operator',
      'type',
    ),

    //
    // expr.
    //

    exprs: $ => seq($.expr, repeat(seq(',', $.expr))),
    expr: $ => choice(
      // No associativity.
      $.id,
      seq('(', $.expr, ')'),
      // Right associavity.
      prec.right(1, seq('(', $.type_name, ')', $.expr)),
      prec.right(1, seq($.id, '(', repeat($.expr), ')')),
      prec.right(1, seq($.prefix_op, prec(2, $.expr))),
      // Left associativity.
      prec.left(1, seq($.expr, '?', $.expr, ':', $.expr)),
      prec.left(1, seq($.expr, choice($.infix_op, $.mapped_op, $.assign_op), $.expr)),
      prec.left(1, seq($.expr, '[', $.subscript, ']')),
      prec.left(1, seq($.expr, '.', $.id)),
      prec.left(1, seq($.expr, $.postfix_op)),
    ),

    prefix_op: _ => choice('!', '-', '~', '++', '--'),
    infix_op: _ => choice('+', '-', '*', '/', '%', '<<', '>>', '&', 'ˆ', '|', '&&', '||', 'in', '<', '<=', '>', '>=', '!=', '==', '?:'),
    mapped_op: _ => choice('.+', '.-', '.*', './', '.%', '.<<', '.>>', '.&', '.^', '.|', '.<', '.<=', '.>', '.>=', '.!=', '.=='),
    assign_op: _ => choice('=', '+=', '-=', '*=', '/=', '%=', '<<=', '>>=', '&=', '^=', '|='),
    postfix_op: _ => choice('++', '--'),

    //
    // stmt.
    //

    stmt: $ => choice(
      $.vardef,
      $.block_stmt,
      $.expr_stmt,
      $.if_stmt,
      $.for_stmt,
      $.while_stmt,
      $.break_stmt,
      $.cont_stmt,
      $.ret_stmt,
    ),

    vardef: $ => seq(optional('mutable'), $.type, seq($.vardef_assign, repeat(seq(',', $.vardef_assign)))),
    vardef_assign: $ => seq($.id, optional(seq('=', $.expr))),

    block_stmt: $ => seq('{', repeat(choice($.stmt, $.standalone_typedef)), '}'),
    expr_stmt: $ => seq($.expr, ';'),
    if_stmt: $ => prec.left(seq('if', '(', $.expr, ')', $.stmt, optional(seq('else', $.stmt)))),
    for_stmt: $ => seq('for', '(', $.type, $.id, 'in', $.expr, ')', $.stmt),
    while_stmt: $ => seq('while', '(', $.expr, ')', $.stmt),
    break_stmt: _ => seq('break', ';'),
    cont_stmt: _ => seq('continue', ';'),
    ret_stmt: $ => seq('return', optional($.expr), ';'),

    //
    // misc.
    //

    annots: $ => repeat1($.annot),
    annot: $ => seq('@', $.id),

    attrs: $ => seq($.attr, repeat(seq(',', $.attr))),
    attr: $ => seq($.type, $.id),

    exts: $ => seq($.ext, repeat(seq(',', $.ext))),
    ext: $ => choice($.id, $.tuple_type),

    id: _ => /\$?[a-zA-Z_][a-zA-Z_0-9]*/,

    lit: $ => choice(
      $.list_lit,
      // $.curly_lit,
      $.primitive_lit),
    list_lit: $ => seq('[', optional($.expr), ']'),
    // curly_lit: $ => seq('{', optional($.expr_or_mapping), '}'),

    primitive_lit: _ => choice(
      'true',
      'false',
      // $.string_lit,
      // $.xml_lit,
      // $.float_lit,
      // $.int_lit,
      // $.hex_lit,
    ),

    subscript: $ => choice($.expr, seq(optional($.expr), ':', optional($.expr))),

    type_lit: $ => choice(
      $.primitive_type,
      // $.comp_type,
      // $.optional_type,
    ),

  }

});
