-file("./site/modal.erl.tmp", 1).

-module(modal).

-compile(export_all).

main(In) ->
    Var@1 =
        #{html =>
              <<"\n<h1>Modal</h1>\n<p>What this space - not yet impleme"
                "nted">>},
    app_compiler_vsn2:combine_fragments([Var@1]).



