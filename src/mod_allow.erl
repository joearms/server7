-module(mod_allow).

-uuid("f61cac7d-8f7c-4a57-963d-a56f1c60a8d6").

-tags([allow,authorisation,web_server]).

-description("Return a list of allowed modules that can be called as CGI scripts from the berwser").


-export([allow/0]).

allow() ->
    [mod_get_tags, mod_full_text_search].
