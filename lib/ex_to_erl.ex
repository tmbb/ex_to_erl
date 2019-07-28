defmodule ExToErl do
  @moduledoc """
  Utilities to convert Elixir expressions into the corresponding Erlang.

  This package is meant to be used as a learning tool or as part of development workflow.
  It was written to answer questions like: "What does this Elixir expression compile to?".
  It's very useful to explore the output of the Elixir compiler in a user-friendly way.

  One should be careful when using this in production with user supplied input
  because most functions in this module run the Elixir compiler and generate atoms
  dynamically at runtime (as the Elixir compiler does).

  The code might also be victim of race conditions (I haven't tested running it in parallel, though).

  It has no tests yet, but I hope it will have some in the future.
  The API will probably change a lot.
  I might switch from raising errors to returning `{:ok, value}` and `:error`.
  """

  @sandbox_module ExToEarl.Sandboxes.ElixirExpressionCompilerSandbox

  @doc """
  Extracts the Erlang abstract code from a BEAM module.

  The argument to this function can be either:

  - The module name (an atom)
  - A `{:module, module, binary, _}` tuple, returned by `Module.create/3`
  - The `binary` part from the tuple above

  ## Examples

  TODO
  """
  def beam_to_erlang_abstract_code(module) do
    beam =
      case module do
        module when is_atom(module) ->
          :code.which(module)

        {:module, _, binary, _} when is_binary(binary) ->
          binary
      end

    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} = :beam_lib.chunks(beam, [:abstract_code])

    abstract_code
  end

  @doc """
  Extracts the Erlang abstract code from a BEAM module and converts it
  into Erlang source code.

  The argument to this function can be either:

  - The module name (an atom)
  - A `{:module, module, binary, _}` tuple, returned by `Module.create/3`
  - The `binary` part from the tuple above

  ## Examples

      iex> module = Module.create(MyModule, quote(do: def f(x) do x end), __ENV__)
      {:module, MyModule,
      <<70, 79, 82, 49, 0, 0, 3, 220, 66, 69, 65, 77, 65, 116, 85, 56, 0, 0, 0, 124,
        0, 0, 0, 13, 15, 69, 108, 105, 120, 105, 114, 46, 77, 121, 77, 111, 100, 117,
        108, 101, 8, 95, 95, 105, 110, 102, 111, ...>>, {:f, 1}}

      iex> ExToErl.beam_to_erlang_abstract_code(module)
      [
        {:attribute, 6, :file, {'iex', 6}},
        {:attribute, 6, :module, MyModule},
        {:attribute, 6, :compile, [:no_auto_import]},
        {:attribute, 6, :export, [__info__: 1, f: 1]},
        {:attribute, 6, :spec,
        {{:__info__, 1},
          [
            {:type, 6, :fun,
            [
              {:type, 6, :product,
                [
                  {:type, 6, :union,
                  [
                    {:atom, 6, :attributes},
                    {:atom, 6, :compile},
                    {:atom, 6, :functions},
                    {:atom, 6, :macros},
                    {:atom, 6, :md5},
                    {:atom, 6, :module},
                    {:atom, 6, :deprecated}
                  ]}
                ]},
              {:type, 6, :any, []}
            ]}
          ]}},
        {:function, 0, :__info__, 1,
        [
          {:clause, 0, [{:atom, 0, :module}], [], [{:atom, 0, MyModule}]},
          {:clause, 0, [{:atom, 0, :functions}], [],
            [{:cons, 0, {:tuple, 0, [{:atom, 0, :f}, {:integer, 0, 1}]}, {nil, 0}}]},
          {:clause, 0, [{:atom, 0, :macros}], [], [nil: 0]},
          {:clause, 0, [{:match, 0, {:var, 0, :Key}, {:atom, 0, :attributes}}], [],
            [
              {:call, 0,
              {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :get_module_info}},
              [{:atom, 0, MyModule}, {:var, 0, :Key}]}
            ]},
          {:clause, 0, [{:match, 0, {:var, 0, :Key}, {:atom, 0, :compile}}], [],
            [
              {:call, 0,
              {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :get_module_info}},
              [{:atom, 0, MyModule}, {:var, 0, :Key}]}
            ]},
          {:clause, 0, [{:match, 0, {:var, 0, :Key}, {:atom, 0, :md5}}], [],
            [
              {:call, 0,
              {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :get_module_info}},
              [{:atom, 0, MyModule}, {:var, 0, :Key}]}
            ]},
          {:clause, 0, [{:atom, 0, :deprecated}], [], [nil: 0]}
        ]},
        {:function, 6, :f, 1,
        [{:clause, 6, [{:var, 6, :__@1}], [], [{:var, 6, :__@1}]}]}
      ]
  """
  def beam_to_erlang_source(module) do
    abstract_code = beam_to_erlang_abstract_code(module)
    erlang_abstract_code_to_string(:erl_syntax.form_list(abstract_code))
  end

  @doc """
  Extracts the Erlang abstract code from a BEAM module, converts it
  into Erlang source code and writes it into a file.

  The first argument to this function can be either:

  - The module name (an atom)
  - A `{:module, module, binary, _}` tuple, returned by `Module.create/3`
  - The `binary` part from the tuple above

  ## Examples

      iex> module = Module.create(MyModule, quote(do: def f(x) do x end), __ENV__)
      {:module, MyModule,
      <<70, 79, 82, 49, 0, 0, 3, 220, 66, 69, 65, 77, 65, 116, 85, 56, 0, 0, 0, 124,
        0, 0, 0, 13, 15, 69, 108, 105, 120, 105, 114, 46, 77, 121, 77, 111, 100, 117,
        108, 101, 8, 95, 95, 105, 110, 102, 111, ...>>, {:f, 1}}

      iex> ExToErl.beam_to_erlang_source(module) |> IO.puts()
      -file("iex", 3).

      -module('Elixir.MyModule').

      -compile([no_auto_import]).

      -export(['__info__'/1, f/1]).

      -spec '__info__'(attributes | compile | functions |
                      macros | md5 | module | deprecated) -> any().

      '__info__'(module) -> 'Elixir.MyModule';
      '__info__'(functions) -> [{f, 1}];
      '__info__'(macros) -> [];
      '__info__'(Key = attributes) ->
          erlang:get_module_info('Elixir.MyModule', Key);
      '__info__'(Key = compile) ->
          erlang:get_module_info('Elixir.MyModule', Key);
      '__info__'(Key = md5) ->
          erlang:get_module_info('Elixir.MyModule', Key);
      '__info__'(deprecated) -> [].

      f(__@1) -> __@1.

      :ok
  """
  def beam_to_erlang_source(module, filename) do
    contents = beam_to_erlang_source(module)
    File.write(filename, contents)
  end

  @doc """
  Converts a string containing Elixir code into an Erlang expression.

  This function expects an Elixir expression.
  If you supply a block (which is a valid Elixir expression), only the last one
  will be converted into an Erlang expression.
  This limitation is a result of the fact that in Erlang a sequence of instructions
  if not a an Erlang expression (on the other hand, a sequence of Elixir
  expressions is an Elixir expression).
  Don't use this function to convert entire Elixir modules to Erlang.
  Use `ExToErl.beam_to_erlang_source/1` instead.

  The function raises if the string is not valid Elixir.
  As with most functions in this module, this function *creates atoms at runtime*
  because valid Erlang AST contains atoms.

  ## Examples

  Single expressions:

      iex> ExToErl.elixir_source_to_erlang_abstract_code("a + b")
      {:op, 1, :+, {:var, 1, :_a@1}, {:var, 1, :_b@1}}

      iex> ExToErl.elixir_source_to_erlang_abstract_code("a <= b")
      {:op, 1, :"=<", {:var, 1, :_a@1}, {:var, 1, :_b@1}}

  Elixir blocks (only the last expression is returned):

      iex> ExToErl.elixir_source_to_erlang_abstract_code("_ = a + b; c + d")
      {:op, 1, :+, {:var, 1, :_c@1}, {:var, 1, :_d@1}}

  You can import functions and macros inside your Elixir expression:

      iex> ExToErl.elixir_source_to_erlang_abstract_code("import Bitwise; a >>> b")
      {:op, 1, :bsr, {:var, 1, :_a@1}, {:var, 1, :_b@1}}

      iex> ExToErl.elixir_source_to_erlang_abstract_code("import Bitwise; a &&& b")
      {:op, 1, :band, {:var, 1, :_a@1}, {:var, 1, :_b@1}}

  Some expressions may raise warnings, although they should be the same wanings
  as if the Elixir expression were to be compiled inside a normal Elixir module:

      iex> ExToErl.elixir_source_to_erlang_abstract_code("a = b")
      warning: variable "a" is unused

      warning: variable "a" is unused

      {:match, 1, {:var, 1, :_a@2}, {:var, 1, :_b@1}}

  Some Elixir operators are actually macros or special forms which can be expanded
  into quite complex Erlang code:

      iex> ExToErl.elixir_source_to_erlang_abstract_code("a or b")
      {:case, 1, {:var, 1, :_a@1},
      [
        {:clause, [generated: true, location: 1], [{:atom, 0, false}], [],
          [{:var, 1, :_b@1}]},
        {:clause, [generated: true, location: 1], [{:atom, 0, true}], [],
          [{:atom, 0, true}]},
        {:clause, [generated: true, location: 1], [{:var, 1, :__@1}], [],
          [
            {:call, 1, {:remote, 1, {:atom, 0, :erlang}, {:atom, 1, :error}},
            [{:tuple, 1, [{:atom, 0, :badbool}, {:atom, 0, :or}, {:var, 1, :__@1}]}]}
          ]}
      ]}

  """
  def elixir_source_to_erlang_abstract_code(elixir) do
    ast = Code.string_to_quoted!(elixir)
    elixir_ast_to_erlang_abstract_code(ast)
  end

  @doc ~S"""
  Converts a string containing Elixir code into Erlang source code.

  This function expects an Elixir expression.
  If you supply a block (which is a valid Elixir expression), only the last one
  will be converted into an Erlang expression.
  This limitation is a result of the fact that in Erlang a sequence of instructions
  if not a an Erlang expression (on the other hand, a sequence of Elixir expressions
  is an Elixir expression).
  Don't use this function to convert entire Elixir modules to Erlang source code.
  Use `ExToErl.beam_to_erlang_source/1` instead.

  The function raises if the string is not valid Elixir.
  As with most functions in this module, this function *creates atoms at runtime*
  because valid Erlang AST contains atoms.

  ## Examples

      iex> ExToErl.elixir_source_to_erlang_source("a")
      "_a@1\n"

      iex> ExToErl.elixir_source_to_erlang_source("a + b")
      "_a@1 + _b@1\n"

      iex> ExToErl.elixir_source_to_erlang_source("a + b < f.(x)")
      "_a@1 + _b@1 < _f@1(_x@1)\n"

      iex> ExToErl.elixir_source_to_erlang_source("a or b") |> IO.puts()
      case _a@1 of
        false -> _b@1;
        true -> true;
        __@1 -> erlang:error({badbool, 'or', __@1})
      end

      :ok

      iex(3)> ExToErl.elixir_source_to_erlang_source("a.b") |> IO.puts()
      case _a@1 of
        #{b := __@1} -> __@1;
        __@1 when erlang:is_map(__@1) ->
            erlang:error({badkey, b, __@1});
        __@1 -> __@1:b()
      end

      :ok
  """
  def elixir_source_to_erlang_source(elixir) do
    abstract_code = elixir_source_to_erlang_abstract_code(elixir)
    erlang_abstract_code_to_string(abstract_code)
  end

  @doc """
  Converts Elixir AST into Erlang abstract code.

  This function expects an Elixir expression.
  If you supply a block (which is a valid Elixir expression), only the last one
  will be converted into an Erlang expression.
  This limitation is a result of the fact that in Erlang a sequence of instructions
  if not a an Erlang expression (on the other hand, a sequence of Elixir expressions
  is an Elixir expression).

  As with most functions in this module, this function *creates atoms at runtime*
  because valid Erlang AST contains atoms.

  ## Examples

      iex> ExToErl.elixir_ast_to_erlang_abstract_code({:+, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]})
      {:op, 1, :+, {:var, 1, :_a@1}, {:var, 1, :_b@1}}

      iex> Code.string_to_quoted!("a - b") |> ExToErl.elixir_ast_to_erlang_abstract_code()
      {:op, 1, :-, {:var, 1, :_a@1}, {:var, 1, :_b@1}}
  """
  def elixir_ast_to_erlang_abstract_code(ast) do
    variables = extract_variables_from_elixir_ast(ast)

    module_body =
      quote do
        @moduledoc "Just a temporary place to store some Erlang abstract code"
        def main(unquote_splicing(variables)) do
          unquote(ast)
        end
      end

    {:module, module_name, _, _} = module = Module.create(@sandbox_module, module_body, __ENV__)

    full_module_abstract_code = beam_to_erlang_abstract_code(module)
    function = find_function_by_name(full_module_abstract_code, :main)
    body = extract_body_from_function_clause(function)

    # Delete the module to avoid the annoying warning about redefining modules.
    # Because functions in this module will never be called, there's no need to purge the module.
    :code.purge(module_name)
    true = :code.delete(module_name)

    body
  end

  @doc """
  Parses an Erlang expression into erlang abstract code.

  ## Examples

      iex> ExToErl.erlang_source_to_abstract_code("A + B.")
      {:op, 1, :+, {:var, 1, :A}, {:var, 1, :B}}

      iex> ExToErl.erlang_source_to_abstract_code("A < B.")
      {:op, 1, :<, {:var, 1, :A}, {:var, 1, :B}}

      iex> ExToErl.erlang_source_to_abstract_code("A + B * C < F + G.")
      {:op, 1, :<,
        {:op, 1, :+, {:var, 1, :A}, {:op, 1, :*, {:var, 1, :B}, {:var, 1, :C}}},
        {:op, 1, :+, {:var, 1, :F}, {:var, 1, :G}}}

      iex> ExToErl.erlang_source_to_abstract_code("A + B * C < f(x) + g(y).")
      {:op, 1, :<,
        {:op, 1, :+, {:var, 1, :A}, {:op, 1, :*, {:var, 1, :B}, {:var, 1, :C}}},
        {:op, 1, :+, {:call, 1, {:atom, 1, :f}, [{:atom, 1, :x}]},
          {:call, 1, {:atom, 1, :g}, [{:atom, 1, :y}]}}}

      iex(9)> ExToErl.erlang_source_to_abstract_code("A + B * C < f(X) + g(Y).")
      {:op, 1, :<,
        {:op, 1, :+, {:var, 1, :A}, {:op, 1, :*, {:var, 1, :B}, {:var, 1, :C}}},
        {:op, 1, :+, {:call, 1, {:atom, 1, :f}, [{:var, 1, :X}]},
          {:call, 1, {:atom, 1, :g}, [{:var, 1, :Y}]}}}

  """
  def erlang_source_to_abstract_code(bin) do
    charlist = String.to_charlist(bin)
    {:ok, tokens, _} = :erl_scan.string(charlist)
    {:ok, [expression]} = :erl_parse.parse_exprs(tokens)
    expression
  end

  @doc """
  Pretty prints Erlang abstract code as Erlang source code.

  ## Examples

  TODO
  """
  def erlang_abstract_code_to_string(abstract_code, opts \\ []) do
    indent = Keyword.get(opts, :indent, 8)

    [:erl_prettypr.format(abstract_code), "\n"]
    |> to_string()
    |> String.replace("\t", String.duplicate(" ", indent))
  end

  # ----------------------------------
  # Private functions
  # ----------------------------------

  defp find_function_by_name(forms, name) do
    Enum.find(forms, fn form ->
      case form do
        {:function, _line, ^name, _arity, clauses} when is_list(clauses) ->
          true

        _ ->
          false
      end
    end)
  end

  # Extracts the list of variables form an Elixir AST fragment
  defp extract_variables_from_elixir_ast(ast) do
    {_ast, variables} =
      Macro.postwalk(ast, [], fn ast_node, variables ->
        case ast_node do
          {name, _meta, module} = variable when is_atom(name) and is_atom(module) ->
            {ast_node, [variable | variables]}

          _other ->
            {ast_node, variables}
        end
      end)

    variables
  end

  defp extract_body_from_function_clause({:function, _line, _name, _arity, [clause]}) do
    {:clause, _line, _args, _guards, body} = clause
    List.last(body)
  end
end
