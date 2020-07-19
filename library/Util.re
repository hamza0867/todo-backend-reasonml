let get_with_default = (default, option_a) =>
  switch (option_a) {
  | Some(x) => x
  | None => default
  };
