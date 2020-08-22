module type Connection = {
  let pool: Caqti_lwt.Pool.t(Caqti_lwt.connection, [> Caqti_error.connect]);

  type error =
    | Database_error(string);
};

module Connection = {
  let connection_url =
    try(Sys.getenv("CONN_STRING")) {
    | Not_found => failwith("CONN_STRING env variable not found")
    };

  let pool =
    switch (
      Caqti_lwt.connect_pool(~max_size=10, Uri.of_string(connection_url))
    ) {
    | Ok(pool) => pool
    | Error(err) => failwith(Caqti_error.show(err))
    };

  type error =
    | Database_error(string);
};
