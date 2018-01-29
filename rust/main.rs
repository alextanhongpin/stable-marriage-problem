use std::collections::HashMap;
// -- Expected results:
// -- Married {male = "2", female = "4"}
// -- Married {male = "1", female = "5"}
// -- Married {male = "3", female = "6"}
// -- Married {male = "0", female = "7"}
fn main() {
	let mut males = vec![
    ("0", vec!["7", "5", "6", "4"]),
    ("1", vec!["5", "4", "6", "7"]),
    ("2", vec!["4", "5", "6", "7"]),
    ("3", vec!["4", "5", "6", "7"])];

  let females = vec![
    ("4", vec!["0", "1", "2", "3"]),
    ("5", vec!["0", "1", "2", "3"]),
    ("6", vec!["0", "1", "2", "3"]),
    ("7", vec!["0", "1", "2", "3"])];

	let score_table = make_score_table(females);
	println!("score_table: {:?}", score_table);
	let mut relationship: HashMap<&'static str, &'static str> = HashMap::new();

	let mut score = 0;
	while score != males.len() {
		for &mut (male, ref mut choices) in males.iter_mut() {
			let is_engaged = match relationship.get(male) {
				Some(_) => true,
				None => false,
			};
			if is_engaged {
				continue;
			}
			if let Some((head, elements)) = choices.clone().split_first_mut() {
				*choices = Vec::from(elements);
				let female = *head;
				if female == "" {
					println!("no more females!");
					continue;
				}
				let curr_male = match relationship.get(female) {
					Some(curr_male) => curr_male,
					None => "",
				};
				if curr_male == "" {
					score += 1;
					*relationship.entry(male).or_insert(female) = female;
					*relationship.entry(female).or_insert(male) = male;
				} else {
					let score1 = lookup_score(male, female, &score_table);
					let score2 = lookup_score(curr_male, female, &score_table);
					if score1 < score2 {
						*relationship.entry(male).or_insert(female) = female;
						*relationship.entry(female).or_insert(male) = male;
						*relationship.entry(curr_male).or_insert("") = "";
					}
				}
			}
		}
	}
	println!("relationship = {:?}", relationship);
	println!("males = {:?}", males);
	// println!("score = {:?}", score);
}

fn make_score_table(preferences: Vec<(&'static str, Vec<&'static str>)>) -> HashMap<&'static str, HashMap<&'static str, i32>> {
	let mut scores = HashMap::new();
	for (female, choices) in preferences {
		let mut ranking = HashMap::new();
		let mut score = 0;
		for male in choices {
			ranking.insert(male, score);
			score += 1;
		}
		scores.insert(female, ranking);
	}
	return scores
}

fn lookup_score(male: &'static str, female: &'static str, score_table: &HashMap<&'static str, HashMap<&'static str, i32>>) -> i32 {
	match score_table.get(female) {
		Some(scores) => match scores.get(male) {
			Some(score) => *score,
			None => i32::max_value(),
		},
		None => i32::max_value(),
	}
}
