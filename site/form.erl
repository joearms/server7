-file("./site/form.erl.tmp", 1).

-module(form).

-compile(export_all).

main(In) ->
    Var@1 = #{html => <<"\n\n">>},
    Var@2 =
        begin
            make_form:make_form([#{type => hidden,
                                   name => "fid",
                                   value => "abcdef124"},
                                 #{type => text,
                                   label => "Name:*",
                                   name => "name",
                                   value => "fred"},
                                 #{type => password,
                                   label => "Password:*",
                                   name => "password",
                                   value => "abc"},
                                 #{type => select,
                                   label => "Birth:",
                                   name => "birth",
                                   values =>
                                       ["Aquaries","Pisces",select]},
                                 #{type => textarea,
                                   label => "What does your program do:",
                                   name => "text",
                                   rows => 5,
                                   value => "nothing"},
                                 #{type => checkboxes,
                                   label => "What do you like to do:",
                                   values =>
                                       [{checked,"cycling"},
                                        "fishing",
                                        nl,
                                        "hiking",
                                        "swimming"]},
                                 #{type => radio,
                                   label => "Color:",
                                   name => "color",
                                   values =>
                                       ["red",
                                        {checked,"green"},
                                        "white",
                                        "red"]}])
        end,
    Var@3 = #{html => <<"\n\n">>},
    app_compiler_vsn2:combine_fragments([Var@1,Var@2,Var@3]).



