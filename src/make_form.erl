-module(make_form).

-compile(export_all).

-keywords([form,generate,erlanf,html]).
-description("Generate an HTML from from an erlang description of the form").

test() ->
    H = make_form(form()),
    elib1_misc:check_io_list(H),
    ok = file:write_file("tmp_auto_generate_form.html", [css(),H]).

css() ->
   "<style> 
form.myform table {
  border:0;
  padding:2;
  spacing:1; 
  width:600; 
  bgcolor:#efefef;
  border: 1px solid #777777;
}
  
form.myform td {
  vertical-align:top;
  background-color:#cdcdcd;
}


form.myform input {
    width:100%;
}

form.myform td textarea {
    width:100%;

}</style>".


form() ->
    [
     #{type => hidden, name => "fid", value => "abcdef124"},
     #{type => text, label => "Name:*", name => "name", value => "fred"},
     #{type => password, label => "Password:*", name => "password", value => "abc"},
     #{type => select,label => "Birth:", name => "birth", values => ["Aquaries", "Pisces", select]},
     #{type => textarea, label => "What does your program do:",name => "text", rows => 5, value => "nothing"},
     #{type => checkboxes, label => "What do you like to do:",
       values => [{checked,"cycling"},"fishing",nl,"hiking","swimming"]},
     #{type => radio, label => "Color:",name => "color", values => ["red",{checked, "green"},"white","red"]}
    ].

make_ran() ->
    {_,_,T} = erlang:now(),
    "form" ++ integer_to_list(T).
  
make_form(F) ->
    %% make a random number from the time
    Ran = make_ran(),
    {Hidden, Rows} = lists:partition(fun is_hidden/1, F),
    Html = [css(),
	    "<form class='myform' id='",Ran,"' onsubmit='return false;'>\n",
	    [hide(I) || I <- Hidden],
	    "<table>\n"
	    "<colgroup align='left' width='30%'>\n"
	    "<colgroup align='left' width='70%'>\n",
	    [row(I) || I <- Rows],
	    final_buttons(Ran),
	    "</table>\n"
	    "</form>",
	    "<script>
function get_radio(r)
{
    for(var i = 0; i < r.length; i++)
    {
	if(r[i].checked)return r[i].value;
    }
    return '';
}
</script>",
     make_read_code(F)],
     #{html => Html}.

is_hidden(#{type := hidden}) -> true;
is_hidden(_) -> false.


final_buttons(Ran) ->
    ["<tr>
       <td colspan='2'>  
	 <button><a href='#'>Close</a></button>
	 <button><a href='#' onclick='read_me(\"",Ran,"\")' >Submit</a></button> 
       </td> 
      </tr>"]. 

hide(#{type := hidden, name := Name, value := Text}) ->
    ["<input type='hidden' name='",Name,"' value='",Text,"'/>\n"].

row(#{type := text,label := Prompt,name := Name,value := Val}) ->
    row2(Prompt, 
	 ["<input class='readme' type='text' name='",Name,"' value='",Val,"' />\n"]);
row(#{type := password, label := Prompt,name := Name, value := Val}) ->
    row2(Prompt, 
	 ["<input class='password' type='text' name='",Name,"' value='",Val,"' />\n"]);
row(#{type := select, label:= Prompt, name := Name, values := Options}) ->
    row2(Prompt,
	 ["<select class='readme' name='",Name,"'>\n",
	  [option(I) || I <- Options],
	  "</select>"]);
row(#{type:=textarea,label:=Prompt,name := Name,rows := Rows,value := Text}) ->
    row2(Prompt,
	 ["<textarea class='readme' name='",
	  Name,"' rows='",
	  Rows,
	  "'>\n",
	  Text,
	  "</textarea>\n"]);
row(#{type := checkboxes, label := Prompt, values := L}) ->
    row2(Prompt,
	 [checkbox(I) || I <- L]);
row(#{type := radio, label := Prompt, name := Name, values := L}) ->
    row2(Prompt,
	 [radio(I, Name) || I <- L]).


default_read(Name) ->
    ["{name:'", Name,"',value:f.elements['",Name,"'].value},\n"].

read_values({checked,Name}) -> default_read(Name);
read_values(nl)             -> [];
read_values(Name)           -> default_read(Name).

option(select) ->
    option({select,"-- Choose an option --"});
option({select, Str}) ->
    ["<option value='",Str,"' selected=''>",Str,"</option>"];
option(Str) ->
    ["<option value='",Str,"'>",Str,"</option>"].

checkbox(nl) -> "<br/>\n";
checkbox({checked,Str}) -> 
    ["<input type='checkbox' style='width:20px' name='",Str,"' checked='checked'/>",Str,"\n"];
checkbox(Str) -> 
    ["<input type='checkbox' style='width:20px' name='",Str,"'/>",Str,"\n"].

radio(br, _) -> "<br/>\n";
radio({checked,Str}, Name) -> 
    ["<input type='radio' style='width:20px' name='",Name,
     "' checked='checked'/>",Str,"\n"];
radio(Str, Name) -> 
    ["<input type='radio' style='width:20px' name='",Name,
     "'/>",Str,"\n"].

row2(X,Y) ->
    ["<tr>\n<td>",X,"</td>\n<td>",Y,"</td>\n</tr>\n"].


read_data(#{type := checkboxes, values := V}) ->
    [read_values(I) || I <- V];
read_data(#{type := radio, name := Name}) ->
    ["{name:'",Name,"',value:get_radio(f.elements['",Name,"'])},\n"];
read_data(#{name := Name}) ->
    default_read(Name).


make_read_code(L) ->
    ["<script>
function read_me(x){
   var f = document.getElementById(x);
   var data = [\n", 
     [read_data(I) || I <- L],
"];
console.log('read', data);
}
</script>
"].





		      
