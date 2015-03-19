-module(esimple_http_handler).

-uuid("5fd10a82-bc6f-495d-aa33-da543b16a615").

-tags([http,erlang]).

-description("http handler for the webserver").

-compile(export_all).

-define(trace(X),(io:format("~p:~p ~p~n",[?MODULE,?LINE,X]))).
    
init(Req, #{request_handler := R, port := Port} = Env) ->
    %% ?trace({req,Req}),
    Path = cowboy_req:path(Req),
    Args = cowboy_req:parse_qs(Req),
    Qs   = cowboy_req:qs(Req),

    File0 = binary_to_list(Path),
    %% expand_escapes expands %20 etc in the file name into blanks
    File = expand_escapes(File0),
    ?trace({file,File,path,Path,args,Args,qs,Qs}),
    %% F is the file reader
    case R(File, Args, Req) of
	{ok, Type, Bin} ->
	    Req1 = cowboy_req:reply(200, [
					  {<<"content-type">>, 
					   mime_type(Type)}
					 ], 
				    Bin, Req),
	    {ok, Req1, Env};
	{error, ZZ} -> 
	    io:format("** missing:~p ~p~n",[File,ZZ]),
	    PortStr = integer_to_list(Port),
	    Reply = list_to_binary(["<h1>no such file</h1>",
				    File," does not exist",
				    "<p>Try <a href='http://localhost:",
				    PortStr,"/index.html'>index.html</a>"]),
	    Req1 = cowboy_req:reply(200, [
					  {<<"content-type">>, 
					   mime_type(".html")}
					 ], 
				    Reply,
				    Req),
	    {ok, Req1, Env}
    end.

%% quick hack

expand_escapes("%20" ++ T) -> " " ++ expand_escapes(T);
expand_escapes([H|T])      -> [H|expand_escapes(T)];
expand_escapes([])         -> [].

mime_type_of_file(F) ->
    mime_type(filename:extension(F)).

mime_type(".ehe")     -> "text/html";
mime_type(".md")      -> "text/html";
mime_type(".gif")     -> "image/gif";
mime_type(".ico")     -> "image/x-icon";
mime_type(".jpg")     -> "image/jpeg";
mime_type(".png")     -> "image/png";
mime_type(".css")     -> "text/css";
mime_type(".special") -> "text/plain; charset=x-user-defined";
mime_type(".json")    -> "application/json";
mime_type(".swf")     -> "application/x-shockwave-flash";
mime_type(".html")    -> "text/html";
mime_type(".xul")     -> "application/vnd.mozilla.xul+xml";
mime_type(".js")      -> "application/x-javascript";
mime_type(".svg")     -> "image/svg+xml";
mime_type(_)          -> "text/plain".
 
