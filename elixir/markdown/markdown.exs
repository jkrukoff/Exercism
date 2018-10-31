defmodule Markdown do
  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

    ## Examples

    iex> Markdown.parse("This is a paragraph")
    "<p>This is a paragraph</p>"

    iex> Markdown.parse("#Header!\n* __Bold Item__\n* _Italic Item_")
    "<h1>Header!</h1><ul><li><em>Bold Item</em></li><li><i>Italic Item</i></li></ul>"
  """
  @spec parse(String.t()) :: String.t()
  def parse(markdown) do
    tokenize(markdown)
    |> parse_tokens()
    |> evaluate()
  end

  @match_tokens ~r/
    (?'header' ^\#+\s*) |
    (?'list' ^\*\s*) |
    (?'bold' __) |
    (?'italic' _) |
    (?'newline' \R) |
    (?'text' .)
  /mux

  @match_names Enum.map(Regex.names(@match_tokens), &String.to_atom/1)

  defp tokenize(markdown) do
    # The single character "text" grouping makes this kind of a ghetto
    # tokenizer, but it's nothing that can't be fixed up in the
    # parser.
    for match <- Regex.scan(@match_tokens, markdown, capture: :all_names) do
      Enum.zip(@match_names, match)
      |> Enum.filter(fn {_name, match} -> match != "" end)
      |> hd()
    end
  end

  defp parse_tokens(tokens, ast \\ [])

  defp parse_tokens([], ast) do
    # End of file.
    Enum.reverse(ast)
  end

  defp parse_tokens([{:newline, _} | tokens], ast) do
    # Blank line.
    parse_tokens(tokens, ast)
  end

  defp parse_tokens([{:header, token} | tokens], ast) do
    # Start of a header line.
    {tokens, header_ast} = parse_line(tokens, [])
    ast = [{:header, String.length(String.trim(token)), header_ast} | ast]
    parse_tokens(tokens, ast)
  end

  defp parse_tokens([{:list, _} | tokens], [{:list, list_ast} | ast]) do
    # Continuation of a list.
    {tokens, item_ast} = parse_line(tokens, [])
    ast = [{:list, list_ast ++ [{:item, item_ast}]} | ast]
    parse_tokens(tokens, ast)
  end

  defp parse_tokens([{:list, _} | tokens], ast) do
    # Start of a list.
    {tokens, item_ast} = parse_line(tokens, [])
    ast = [{:list, [{:item, item_ast}]} | ast]
    parse_tokens(tokens, ast)
  end

  defp parse_tokens(tokens, ast) do
    # Start of a paragraph.
    {unparsed_tokens, paragraph_ast} = parse_line(tokens, [])
    ast = [{:paragraph, paragraph_ast} | ast]
    parse_tokens(unparsed_tokens, ast)
  end

  defp parse_line([], ast) do
    # End of file.
    {[], Enum.reverse(ast)}
  end

  defp parse_line([{:newline, _} | tokens], ast) do
    # End of line.
    {tokens, ast}
  end

  defp parse_line([{:text, token} | tokens], ast) do
    # Literal text.
    {tokens, text} = parse_text(tokens, [token])
    ast = [{:literal, text} | ast]
    parse_line(tokens, ast)
  end

  defp expect(what, tokens) do
    [{^what, _} | tokens] = tokens
    tokens
  end

  defp parse_line([{:bold, _} | tokens], ast) do
    # Bold text.
    {tokens, text} = parse_text(tokens, [])
    tokens = expect(:bold, tokens)
    ast = [{:bold, text} | ast]
    parse_line(tokens, ast)
  end

  defp parse_line([{:italic, _} | tokens], ast) do
    # Italic text.
    {tokens, text} = parse_text(tokens, [])
    tokens = expect(:italic, tokens)
    ast = [{:italic, text} | ast]
    parse_line(tokens, ast)
  end

  defp parse_text([{:text, token} | tokens], text) do
    # Continuation of text.
    parse_text(tokens, [token | text])
  end

  defp parse_text(tokens, text) do
    # End of text.
    {tokens, to_string(Enum.reverse(text))}
  end

  defp wrap(tag, content) do
    ["<#{tag}>", content, "</#{tag}>"]
  end

  defp evaluate(ast, acc \\ [])

  defp evaluate([], acc) do
    to_string(Enum.reverse(acc))
  end

  defp evaluate([{:header, level, header_ast} | ast], acc) do
    evaluate(ast, [wrap("h#{level}", evaluate(header_ast)) | acc])
  end

  defp evaluate([{:list, list_ast} | ast], acc) do
    evaluate(ast, [wrap("ul", evaluate(list_ast)) | acc])
  end

  defp evaluate([{:item, item_ast} | ast], acc) do
    evaluate(ast, [wrap("li", evaluate(item_ast)) | acc])
  end

  defp evaluate([{:paragraph, paragraph_ast} | ast], acc) do
    evaluate(ast, [wrap("p", evaluate(paragraph_ast)) | acc])
  end

  defp evaluate([{:italic, text} | ast], acc) do
    evaluate(ast, [wrap("em", text) | acc])
  end

  defp evaluate([{:bold, text} | ast], acc) do
    evaluate(ast, [wrap("strong", text) | acc])
  end

  defp evaluate([{:literal, text} | ast], acc) do
    evaluate(ast, [text | acc])
  end
end
