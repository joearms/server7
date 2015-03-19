-module(svt_tests).
-compile(export_all).

test(1) ->
    Tree = svg_staves:make(4),
    elib2_misc:dump("tree", Tree).
