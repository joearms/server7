-file("./site/ajax1.erl.tmp", 1).

-module(ajax1).

-compile(export_all).

echo(X) ->
    lib_html:pre(X).

main(In) ->
    Var@1 = #{html => <<"\n\n<!-- load the library -->\n">>},
    Var@2 = begin jslib(void) end,
    Var@3 =
        #{html =>
              <<"\n\n<p>Ajax\n\n<button onclick=\"test()\">test</button"
                ">\n\n<div id=\"result\"></div>\n\n<script>\nfunction t"
                "est(){\n    o = document.getElementById('result');\n  "
                "  o.innerHTML += \"<br>test\";\n    callback = functio"
                "n(x){\n    \t     document.getElementById(\"result\")."
                "innerHTML = x;\n\t     };\n    ajax_query('ajax1', 'ec"
                "ho', {a:'hello', b:1}, callback);\t\t\n    }\n</script"
                ">\n">>},
    app_compiler_vsn2:combine_fragments([Var@1,Var@2,Var@3]).

jslib(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<!-- put your js here -->\n\n<script>\n\n// Call t"
                "he erlang function mod:func(Args)\n//   Args will be t"
                "he ERlang representation of JSON message\n//   Wait fo"
                "r a reply from Erlang - then evaluate callback(ReplySt"
                "ring)\n\nfunction ajax_query(mod, func, args, callback"
                "){\n    var xmlhttp;\n    console.log(\"query\", [mod,"
                "func,args]);\n    xmlhttp=new XMLHttpRequest();\n    /"
                "/ note GET fails here ... no data is sent you must use"
                " POST\n    xmlhttp.open(\"POST\",\"weblib?mod=\"+mod+"
                "\"&func=\"+func,true);   \n    xmlhttp.setRequestHeade"
                "r(\"Content-type\",\"application/x-www-form-urlencoded"
                "\");\n    xmlhttp.onreadystatechange=function()\n    {"
                "\n      if (xmlhttp.readyState==4 && xmlhttp.status==2"
                "00)\n      {\n        callback(xmlhttp.responseText);"
                "\n     }\n    };\n    var s = \"string=\" + JSON.strin"
                "gify(args);\n    console.log('s',s);\n    xmlhttp.send"
                "(s);\n}\n\n</script>\n\n\t">>},
    app_compiler_vsn2:combine_fragments([Var@1]).



