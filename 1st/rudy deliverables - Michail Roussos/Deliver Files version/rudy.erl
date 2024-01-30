-module(rudy).
-export([init/1]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            %
            %:
            %pass the socket to the handler
            handler(Listen),

            %

            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            %
            %:
            % pass the incoming connection to the request handler
            request(Client),
            % start a new handler
            handler(Listen);
            %
        {error, Error} ->
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} -> 
            %
            %:
            % parsing of the requsing the http parser
            Request= http:parse_request(Str),
            % 
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

%reply({{get, URI, _}, _, _}) ->
    %timer:sleep(80),
    %http:ok("Hello World "++URI).


reply({{get,URI, _}, _, _}) -> 
%.  try to read the file from the URI
    case file:read_file("files" ++ URI) of
        {ok, File} -> 
%           when the file is found
            http:ok([File]);
        {error, _Reason} -> 
%           when the file is not found
            [$/|File]=URI,
            http:file_not_found("File " ++ File ++ " was not found in the files folder")
    end. 