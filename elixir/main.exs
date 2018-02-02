males = %{
	"0" => ["7", "5", "6", "4"],
	"1" => ["5", "4", "6", "7"],
	"2" => ["4", "5", "6", "7"],
	"3" => ["4", "5", "6", "7"]
}

male_names = ["0", "1", "2", "3"]

females = %{
	"4" => ["0", "1", "2", "3"],
	"5" => ["0", "1", "2", "3"],
	"6" => ["0", "1", "2", "3"],
	"7" => ["0", "1", "2", "3"]
}

IO.inspect males, label: "males"
IO.inspect females, label: "females"

# Create score table to lookup score of the male
scores = Enum.reduce(females, %{}, fn({ f, males }, table) ->
	out = Enum.reduce(Enum.with_index(males), %{}, fn ({m, score}, scores) -> 
		Map.put(scores, m, score)
	end)
	Map.put(table, f, out)
end)

IO.inspect scores, label: "score table"
IO.inspect scores["4"]["3"], label: "score table"

# A dictionary to check if the male is already engaged or not
defmodule StableMarriageProblem do
	# defp is_engaged(true, females) do
	# 	IO.puts "is single"
	# end

	# defp is_engaged(false, females) do
	# 	IO.inspect females
	# 	IO.puts "not single"
	# 	[f | rest] = females
	# end

	def match(_, engaged, _, total_matches) when total_matches === 8 do
		engaged
	end

	def match(init_males, init_engaged, scores, _) do
		# IO.puts total_matches
		{males_up, engaged_up} = Enum.reduce(init_males, {init_males, init_engaged}, fn({ m, choices }, {males, engaged}) ->

			# m 
			# |> (&Map.has_key?(engaged, &1)).()
			# |> is_engaged(choices)

			is_male_single = not Map.has_key?(engaged, m)
			case is_male_single do
				true -> 
					[f | rest] = choices	
					is_female_single = not Map.has_key?(engaged, f)
					engaged = case is_female_single do
						true -> 
							engaged = Map.put(engaged, f, m)
							Map.put(engaged, m, f)
						false ->
							e = Map.get(engaged, f)
							score1 = scores[f][m]
							score2 = scores[f][e]
							case score1 < score2 do
								true ->
									engaged = Map.put(engaged, f, m)
									engaged = Map.put(engaged, m, f)
									Map.put(engaged, e, nil)
								false -> engaged
							end
					end
					males = Map.put(males, m, rest)
					{ males, engaged }
				false -> { males, engaged }
			end

		end)

		new_total_matches = length(Map.keys(engaged_up))
		match(males_up, engaged_up, scores, new_total_matches)
	end
end

matches = StableMarriageProblem.match(males, %{}, scores, 0)
IO.inspect matches, label: "engaged status"


engaged = %{}

defmodule TestTrue do
	def scream(true) do
		IO.puts "is true"
	end

	def scream(false) do
		IO.puts "is falsd"
	end
end