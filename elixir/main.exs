males = %{
	"0" => ["7", "5", "6", "4"],
	"1" => ["5", "4", "6", "7"],
	"2" => ["4", "5", "6", "7"],
	"3" => ["4", "5", "6", "7"]
}

females = %{
	"4" => ["0", "1", "2", "3"],
	"5" => ["0", "1", "2", "3"],
	"6" => ["0", "1", "2", "3"],
	"7" => ["0", "1", "2", "3"]
}

defmodule StableMarriageProblem do
	defp make_score_table(females) do
		score_table = %{}
		Enum.reduce(females, score_table, fn({ f, males }, table) ->
			male_scores = %{}
			out = Enum.reduce(Enum.with_index(males), male_scores, fn ({m, score}, scores) -> 
				Map.put(scores, m, score)
			end)
			Map.put(table, f, out)
		end)
	end

	def match(_, _, final_state, _, is_stable) when is_stable do
		final_state
	end

	def match(init_males, init_females, init_state, scores, _) do
		scores = if scores, do: scores, else: make_score_table(init_females)

		{new_males, new_state} = Enum.reduce(init_males, {init_males, init_state}, fn({m1, choices}, {males, state}) ->
			[f | rest] = choices
			is_male_single = not Map.has_key?(state, m1)
			is_female_single = not Map.has_key?(state, f)

			m2 = if not is_female_single, do: Map.get(state, f), else: nil
			is_preferred = if m2, do: scores[f][m1] < scores[f][m2], else: false

			new_males = Map.put(males, m1, rest)
			truth_table = {is_male_single, is_female_single, is_preferred}

			new_state = case truth_table do
				{true, true, false} ->
					new_state = Map.put(state, f, m1)
					Map.put(new_state, m1, f)
				{true, false, true} ->
					new_state = Map.put(state, f, m1)
					new_state = Map.put(new_state, m1, f)
					Map.put(new_state, m2, nil)
				_ -> state
			end
			{new_males, new_state}
		end)

		total_matches = length(Map.keys(new_state))
		max_matches = length(Map.keys(init_males)) * 2
		is_stable = total_matches === max_matches

		match(new_males, init_females, new_state, scores, is_stable)
	end
end

state = %{}
matches = StableMarriageProblem.match(males, females, state, nil, 0)
IO.inspect matches, label: "matches"