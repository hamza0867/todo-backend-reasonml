module Id: {
  type t;
  let to_yojson: t => Yojson.Safe.t;
  let from_int: int => t;
  let to_int: t => int;
} = {
  [@deriving to_yojson]
  type t = int;
  let from_int = x => x;
  let to_int = x => x;
};

module Todo = {
  [@deriving to_yojson]
  type t = {
    id: Id.t,
    title: string,
    completed: bool,
  };

  let make = (~id, ~title, ~completed) => {id, title, completed};
};

module User = {
  type t = {
    id: Id.t,
    name: string,
    todos: list(Todo.t),
  };

  let make = (~id, ~name, ~todos=[]) => {id, name, todos};
};
