let r: int = 12;
let g: int = 13;
let b: int = 14;

let parse_game_id = (game_id_str: string): int => {
    switch (String.split_on_char(' ', game_id_str)) {
        | ["Game", id_str] => int_of_string(id_str)
        | _ => assert(false)
    }
}

type color =
  | Red
  | Green
  | Blue;

let parse_color = (color_str: string): color => {
    switch (color_str) {
        | "red" => Red
        | "green" => Green
        | "blue" => Blue
        | _ => assert(false)
    }
};

let parse_cube = (cube_str: string): (int, color) => {
    switch (String.split_on_char(' ', cube_str)) {
        | [amount_str, color_str] => {
            let amt = int_of_string(amount_str);
            let color = parse_color(color_str);
            (amt, color)
        }
        | _ => assert(false)
    }
};

let parse_rounds_list = (rounds_str: string): list((int, color)) => {
    Str.split(", " |> Str.regexp_string, rounds_str)
    |> List.map(String.trim)
    |> List.map(parse_cube)
}

let parse_rounds = (rounds_str: string): list(list((int, color))) => {
    String.split_on_char(';', rounds_str)
    |> List.map(parse_rounds_list);
}

let rec is_enough_of_color = (color_counts: list((int, color)), is_possible: bool): bool => {
    switch (is_possible) {
        | true => {
            switch (color_counts) {
                | [(num, Red), ...rest] => num <= r && is_enough_of_color(rest, true)
                | [(num, Blue), ...rest] => num <= b && is_enough_of_color(rest, true)
                | [(num, Green), ...rest] => num <= g && is_enough_of_color(rest, true)
                | [] => is_possible
            }
        }
        | false => false
    }
}

/* For each round, verify count per color is below limit */
let rec is_game_possible = (rounds: list(list((int, color))), is_possible: bool): bool => {
    switch (is_possible) {
        | true => {
            switch (rounds) {
                | [rolls, ...rest] => {
                    is_enough_of_color(rolls, true) && is_game_possible(rest, true)
                }
                | [] => is_possible
            }
        }
        | false => false
    }
}

let parse_game = (line: string): (int, list(list((int, color)))) => {
    switch (String.split_on_char(':', line)) {
        | [game_str, rounds_str] => {
            let game_id = parse_game_id(game_str);
            let rounds = parse_rounds(rounds_str);
            (game_id, rounds)
        }
        | _ => assert(false)
    }
}

let rec part1 = (games: list((int, list(list((int, color))))), sum: int): int => {
    switch (games) {
        | [(id, rounds), ...rest] => {
            switch(is_game_possible(rounds, true)) {
                | true => part1(rest, sum + id)
                | false => part1(rest, sum)
            }
        }
        | [] => sum
    }
}

let rec calc_min = (round: list((int, color)), curr_min: (int, int, int)): (int, int, int) => {
    let (reds, greens, blues) = curr_min;
    switch (round) {
        | [(n, Red), ...rest] => calc_min(rest, (max(reds, n), greens, blues))
        | [(n, Green), ...rest] => calc_min(rest, (reds, max(greens, n), blues))
        | [(n, Blue), ...rest] => calc_min(rest, (reds, greens, max(blues, n)))
        | [] => curr_min
    }
}

let rec calc_mins_rgb = (rounds: list(list((int, color))), curr_mins: (int, int, int)): (int, int, int) => {
    switch (rounds) {
        | [round, ...rest] => {
            let (reds, greens, blues) = calc_min(round, (0, 0, 0));
            let (f_reds, f_greens, f_blues) = calc_mins_rgb(rest, (0, 0, 0));
            (max(reds, f_reds), max(greens, f_greens), max(blues, f_blues))
        }
        | [] => curr_mins
    }
}

let rec part2 = (games: list((int, list(list((int, color))))), sum: int): int => {
    switch (games) {
        | [(id, rounds), ...rest] => {
            let round_mins = calc_mins_rgb(rounds, (0, 0, 0));
            let (reds, greens, blues) = round_mins;
            let power = max(reds, 1) * max(greens, 1) * max(blues, 1);
            part2(rest, sum + power)
        }
        | [] => sum
    }
}

let read_lines = file => {
  let input = open_in(file);
  let rec collect_lines = () => {
    switch (input_line(input)) {
    | line => Seq.Cons(line, collect_lines)
    | exception End_of_file =>
      close_in(input);
      Seq.Nil;
    };
  };

  collect_lines;
};

let games = read_lines("inputs/day2.txt") |> Seq.map(parse_game) |> List.of_seq;

let exec = (): unit => {
    print_endline(string_of_int(part1(games, 0)));
    print_endline(string_of_int(part2(games, 0)));
}