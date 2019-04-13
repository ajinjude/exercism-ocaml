let acronym x =
  String.split_on_chars ~on:[' '; '-'] x
  |> List.map ~f:(.[0])
  |> String.of_char_list
  |> String.uppercase
