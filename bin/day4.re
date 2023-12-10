include FileReader.File_reader;

module IntSet =
  Set.Make({
    type t = int;
    let compare = compare;
  });

let parse_card_id = (card_id_str: string): int => {
    switch (card_id_str |> Str.split("Card" |> Str.regexp_string) |> List.map(String.trim)) {
        | [num] => int_of_string(num)
        | _ => assert(false)
    }
}

let parse_nums = (nums: string): (string, string) => {
    switch (nums |> String.split_on_char('|') |> List.map(String.trim)) {
        | [winning_str, my_str] => (winning_str, my_str);
        | _ => assert(false)
    }
}

let parse_nums_to_set = (nums: string): IntSet.t => {
    nums
    |> String.split_on_char(' ')
    |> List.map(String.trim)
    |> List.filter(x=>x!="")
    |> List.map(int_of_string)
    |> IntSet.of_list;
}

let parse_line = (line: string): (int, IntSet.t, IntSet.t) => {
    switch(line |> String.split_on_char(':') |> List.map(String.trim)) {
        | [card_id_str, nums] => {
            let card_id: int = parse_card_id(card_id_str);
            let (winning_nums_str, my_nums_str) = parse_nums(nums);
            let winning_nums: IntSet.t = parse_nums_to_set(winning_nums_str);
            let my_nums: IntSet.t = parse_nums_to_set(my_nums_str);

            (card_id, winning_nums, my_nums)
        }
        | _ => assert(false)
    }
}

let lines: list((int, IntSet.t, IntSet.t)) = read_lines("inputs/day4.txt")
    |> Seq.map(parse_line)
    |> List.of_seq;

let part1 = (lines: list((int, IntSet.t, IntSet.t))): int => {
    lines
    |> List.map(parsed => {
            let (id, wins, nums) = parsed;
            IntSet.inter(wins, nums) |> IntSet.cardinal
        })
    |> List.map(length => switch (length) { | 0 => 0 | length => int_of_float(2. ** float_of_int((length - 1))) })
    |> List.fold_left((+), 0)
}

let part2 = (lines: list((int, IntSet.t, IntSet.t))): int => {
    let length = List.length(lines);
    let card_counts = Array.make(length, 1);

    lines
    |> List.map(parsed => {
            let (id, wins, nums) = parsed;
            let match_cnt = IntSet.inter(wins, nums) |> IntSet.cardinal;
            // Iterate from card ahead at index i to the last card we'll make a copy of
            for (i in id to id + match_cnt - 1) {
                if (i < length) {
                    // Increment counter with number of current copies of current card
                    card_counts[i] = card_counts[i] + card_counts[id - 1]
                }
            }
        });

    List.fold_left((+), 0, Array.to_list(card_counts))
}

let exec = (): unit => {
    print_endline(string_of_int(part1(lines)));
    print_endline(string_of_int(part2(lines)));
};

exec();