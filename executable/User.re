open ReWeb;
module D = Decoders_yojson.Safe.Decode;

module Entity = {
  [@deriving to_yojson]
  type t = {
    id: Models.Id.t,
    name: string,
    todos: list(Models.Todo.t),
  };

  let make = (~id, ~name) => {id, name, todos: []};

  module Unregistered = {
    type t = {name: string};

    let make = name => {name: name};

    let of_yojson = {
      open D;
      let (<$>) = D.map;
      make <$> field("name", string) |> D.decode_value;
    };
  };

  module Partial = {
    type t = {name: option(string)};

    let make_partial = name => {name: name};

    let of_yojson = {
      open D;
      let (<$>) = D.map;
      make_partial <$> field_opt("name", string) |> D.decode_value;
    };
  };

  let update_with_partial = (partial: Partial.t, user: t): Unregistered.t => {
    name: partial.name |> TodoBackend.Util.get_with_default(user.name),
  };
};

module MakeRepository = (Database: Database.Connection) => {
  open Entity;
  open Database;

  let or_error = m => {
    switch%lwt (m) {
    | Ok(a) => Ok(a) |> Lwt.return
    | Error(e) => Error(Database_error(Caqti_error.show(e))) |> Lwt.return
    };
  };

  let pool = Database.pool;

  let create_table = [%rapper
    execute(
      {sql|
      CREATE TABLE IF NOT EXISTS users (
        id serial,
        name VARCHAR(128) NOT NULL,
        PRIMARY KEY (id)
      );
      |sql},
    )
  ];

  open Lwt_result.Infix;

  Caqti_lwt.Pool.use(create_table(), pool);
  let get_all = () => {
    let get_all_query = [%rapper
      get_many(
        {sql|
        SELECT @int{users.id}, @string{users.name},
        @int?{todos.id}, @string?{todos.title}, @bool?{todos.completed}
        FROM users
          LEFT JOIN todos on todos.user_id = users.id
          ORDER BY users.id;
          |sql},
        function_out,
      )
    ];
    Caqti_lwt.Pool.use(
      get_all_query(
        (
          (~id, ~name) => Entity.make(~id=Models.Id.from_int(id), ~name),
          (~id, ~title, ~completed) =>
            switch (id) {
            | Some(id) =>
              Models.Todo.make(
                ~id=Models.Id.from_int(id),
                ~title=
                  switch (title) {
                  | Some(title) => title
                  | None => ""
                  },
                ~completed=
                  switch (completed) {
                  | Some(completed) => completed
                  | None => false
                  },
              )
            | None =>
              Models.Todo.make(
                ~id=Models.Id.from_int(-1),
                ~title="",
                ~completed=false,
              )
            },
        ),
        (),
      ),
      pool,
    )
    |> or_error
    >|= Rapper.load_many(
          (fst, (Entity.{id, _}) => id),
          [
            (
              snd,
              (user, todos: list(Models.Todo.t)) => {
                ...user,
                todos:
                  todos
                  |> List.filter((todo: Models.Todo.t) =>
                       Models.Id.to_int(todo.id) != (-1)
                     ),
              },
            ),
          ],
        );
  };

  let get_one_by_id = id => {
    let get_one_query = [%rapper
      get_many(
        {sql| SELECT @int{users.id}, @string{users.name},
        @int?{todos.id}, @string?{todos.title}, @bool?{todos.completed}
        FROM users
          LEFT JOIN todos on users.id = todos.user_id
          WHERE users.id = %int{id}
          |sql},
        function_out,
      )
    ];
    Caqti_lwt.Pool.use(
      get_one_query(
        (
          (~id, ~name) => Entity.make(~id=Models.Id.from_int(id), ~name),
          (~id, ~title, ~completed) =>
            switch (id) {
            | Some(id) =>
              Models.Todo.make(
                ~id=Models.Id.from_int(id),
                ~title=
                  switch (title) {
                  | Some(title) => title
                  | None => ""
                  },
                ~completed=
                  switch (completed) {
                  | Some(completed) => completed
                  | None => false
                  },
              )
            | None =>
              Models.Todo.make(
                ~id=Models.Id.from_int(-1),
                ~title="",
                ~completed=false,
              )
            },
        ),
        ~id,
      ),
      pool,
    )
    |> or_error
    >|= Rapper.load_many(
          (fst, (Entity.{id, _}) => id),
          [
            (
              snd,
              (user, todos: list(Models.Todo.t)) => {
                ...user,
                todos:
                  todos
                  |> List.filter((todo: Models.Todo.t) =>
                       Models.Id.to_int(todo.id) != (-1)
                     ),
              },
            ),
          ],
        )
    >>= (
      fun
      | [user] => Lwt.return_ok(Some(user))
      | [] => Lwt.return_ok(None)
      | [_h, ..._t] =>
        Lwt.return_error(Database_error("Internal server error"))
    );
  };

  let create_one = (unregistered: Unregistered.t) => {
    let create_one_query = [%rapper
      execute({sql| INSERT INTO users(name)
    VALUES (%string{name})|sql})
    ];
    let {name}: Unregistered.t = unregistered;
    Caqti_lwt.Pool.use(create_one_query(~name), pool) |> or_error;
  };

  let update_one = (id, unregistered) => {
    let update_one_query = [%rapper
      execute(
        {sql| UPDATE users SET name = %string{name} WHERE id = %int{id}  |sql},
      )
    ];
    let {name}: Unregistered.t = unregistered;
    Caqti_lwt.Pool.use(update_one_query(~id, ~name), pool) |> or_error;
  };
};

open Lwt.Infix;
open Entity;

module Repository = MakeRepository(Database.Connection);

let index = _req =>
  Repository.get_all()
  >>= (
    fun
    | Ok(users) =>
      `List(users |> List.map(to_yojson))
      |> Response.of_json(~status=`OK)
      |> Lwt.return
    | Error(Database_error(e)) => {
        print_endline(e);
        Response.of_text(
          ~status=`Internal_server_error,
          "Internal server error",
        )
        |> Lwt.return;
      }
  );

let show = (id, _req) =>
  switch (int_of_string_opt(id)) {
  | None =>
    Response.of_text(~status=`Bad_request, "id must be an integer")
    |> Lwt.return
  | Some(id) =>
    Repository.get_one_by_id(id)
    >>= (
      fun
      | Ok(maybe_user) =>
        switch (maybe_user) {
        | None =>
          Response.of_text(
            ~status=`Not_found,
            "Could not find user with id = " ++ string_of_int(id),
          )
          |> Lwt.return
        | Some(user) =>
          user |> to_yojson |> Response.of_json(~status=`OK) |> Lwt.return
        }
      | Error(Database_error(e)) => {
          print_endline("\n" ++ e);
          Response.of_text(
            ~status=`Internal_server_error,
            "Internal server error",
          )
          |> Lwt.return;
        }
    )
  };

let create =
  Filter.body_json @@
  (
    req => {
      let json_body: Yojson.Safe.t = Request.context(req);
      switch (Unregistered.of_yojson(json_body)) {
      | Error(e) =>
        Response.of_text(
          ~status=`Bad_request,
          "Error " ++ D.string_of_error(e),
        )
        |> Lwt.return
      | Ok(unregistered) =>
        Repository.create_one(unregistered)
        >>= (
          fun
          | Ok () => Response.of_status(`OK) |> Lwt.return
          | Error(Database_error(e)) => {
              print_endline("\n" ++ e);
              Response.of_text(
                ~status=`Internal_server_error,
                "Internal server error",
              )
              |> Lwt.return;
            }
        )
      };
    }
  );

let update = (_meth, id) =>
  Filter.body_json @@
  (
    req => {
      switch (int_of_string_opt(id)) {
      | None =>
        Response.of_text(
          ~status=`Bad_request,
          "id must be an integer, instead got: " ++ id,
        )
        |> Lwt.return
      | Some(id) =>
        Repository.get_one_by_id(id)
        >>= (
          fun
          | Error(Database_error(e)) => {
              print_endline("\n" ++ e);
              Response.of_text(
                ~status=`Internal_server_error,
                "Internal server error",
              )
              |> Lwt.return;
            }
          | Ok(None) =>
            Response.of_text(
              ~status=`Not_found,
              "There is no user with id: " ++ string_of_int(id),
            )
            |> Lwt.return
          | Ok(Some(persisted)) => {
              let json_body: Yojson.Safe.t = Request.context(req);
              switch (json_body |> Partial.of_yojson) {
              | Error(e) =>
                Response.of_text(
                  ~status=`Bad_request,
                  "Error " ++ D.string_of_error(e),
                )
                |> Lwt.return
              | Ok(partial) =>
                let updated_user = persisted |> update_with_partial(partial);
                Repository.update_one(id, updated_user)
                >>= (
                  fun
                  | Ok () => Response.of_status(`OK) |> Lwt.return
                  | Error(Database_error(e)) => {
                      print_endline("\n" ++ e);
                      Response.of_text(
                        ~status=`Internal_server_error,
                        "Internal server error",
                      )
                      |> Lwt.return;
                    }
                );
              };
            }
        )
      };
    }
  );

let resource = route =>
  Server.resource(~index, ~show, ~create, ~update, route);
