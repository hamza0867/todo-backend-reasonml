open ReWeb;
module D = Decoders_yojson.Safe.Decode;

let server =
  fun
  | (meth, ["todos", ...path]) =>
    ReWeb.Filter.cors(Header.AccessControlAllowOrigin.All) @@
    Todo.resource @@
    (meth, path)
  | (_, path) => (
      _ =>
        Response.of_text(
          ~status=`Not_found,
          "unsupported url "
          ++ List.fold_left((acc, curr) => acc ++ "/" ++ curr, "", path),
        )
        |> Lwt.return
    );

Server.serve(~port=8080, server);
