module type Connection = {
  let pool: Caqti_lwt.Pool.t(Caqti_lwt.connection, [> Caqti_error.connect]);

  type error =
    | Database_error(string);
};

module Connection: Connection;

