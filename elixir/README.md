# Elixir


## Piping with second arguments

```elixir
males
|> (&Map.has_key?(engaged, &1)).()
|> is_engaged(choices)
```