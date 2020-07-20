open ReWeb;
module D = Decoders_yojson.Safe.Decode;

module Entity = {
  [@deriving to_yojson]
  type t = {
    id: int,
    title: string,
    completed: bool,
  };

  module Unregistered = {
    type t = {
      title: string,
      completed: bool,
    };

    let make = (title, completed) => {
      title,
      completed: completed |> TodoBackend.Util.get_with_default(false),
    };

    let of_yojson = {
      open D;
      let (<$>) = D.map;
      make
      <$> field("title", string)
      <*> field_opt("completed", bool)
      |> D.decode_value;
    };
  };

  module Partial = {
    type t = {
      title: option(string),
      completed: option(bool),
    };

    let make_partial = (title, completed) => {title, completed};

    let of_yojson = {
      open D;
      let (<$>) = D.map;
      make_partial
      <$> field_opt("title", string)
      <*> field_opt("completed", bool)
      |> D.decode_value;
    };
  };

  let update_with_partial = (partial: Partial.t, todo: t): Unregistered.t => {
    title: partial.title |> TodoBackend.Util.get_with_default(todo.title),
    completed:
      partial.completed |> TodoBackend.Util.get_with_default(todo.completed),
  };
};

module Repository = {
  open Entity;
  open Database;

  let or_error = m => {
    switch%lwt (m) {
    | Ok(a) => Ok(a) |> Lwt.return
    | Error(e) => Error(Database_error(Caqti_error.show(e))) |> Lwt.return
    };
  };

  let pool = Database.pool;

  let get_all = () => {
    let get_all_query = [%rapper
      get_many(
        {sql| SELECT @int{id}, @string{title}, @bool{completed} FROM todos ORDER BY id|sql},
        record_out,
      )
    ];
    let get_all' = (module C: Caqti_lwt.CONNECTION) =>
      get_all_query((module C), ());
    Caqti_lwt.Pool.use(get_all', pool) |> or_error;
  };

  let get_one_by_id = id => {
    let get_one_query = [%rapper
      get_opt(
        {sql| SELECT @int{id}, @string{title}, @bool{completed} FROM todos WHERE id = %int{id} |sql},
        record_out,
      )
    ];
    let get_one' = (module C: Caqti_lwt.CONNECTION) =>
      get_one_query((module C), ~id);
    Caqti_lwt.Pool.use(get_one', pool) |> or_error;
  };

  let create_one = (unregistered: Unregistered.t) => {
    let create_one_query = [%rapper
      execute(
        {sql| INSERT INTO todos(title, completed)
          VALUES (%string{title}, %bool{completed})|sql},
      )
    ];

    let {title, completed}: Unregistered.t = unregistered;

    let create_one' = (module C: Caqti_lwt.CONNECTION) =>
      create_one_query((module C), ~title, ~completed);

    Caqti_lwt.Pool.use(create_one', pool) |> or_error;
  };

  let update_one = (id, unregistered) => {
    let update_one_query = [%rapper
      execute(
        {sql| UPDATE todos SET (title, completed) = (%string{title}, %bool{completed}) WHERE id = %int{id}  |sql},
      )
    ];
    let {title, completed}: Unregistered.t = unregistered;
    let update_one' = (module C: Caqti_lwt.CONNECTION) =>
      update_one_query((module C), ~title, ~completed, ~id);

    Caqti_lwt.Pool.use(update_one', pool) |> or_error;
  };
};

open Lwt.Infix;
open Entity;

let index = _req =>
  Repository.get_all()
  >>= (
    fun
    | Ok(todos) =>
      `List(todos |> List.map(to_yojson))
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
      | Ok(maybe_todo) =>
        switch (maybe_todo) {
        | None =>
          Response.of_text(
            ~status=`Not_found,
            "Could not find todo with id = " ++ string_of_int(id),
          )
          |> Lwt.return
        | Some(todo) =>
          todo |> to_yojson |> Response.of_json(~status=`OK) |> Lwt.return
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
              "There is no todo with id: " ++ string_of_int(id),
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
                let updated_todo = persisted |> update_with_partial(partial);
                Repository.update_one(id, updated_todo)
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
