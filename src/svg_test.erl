-module(svg_test).
-compile(export_all).

piano() ->
    %% insert svg
    "<svg xml:space='preserve' width='161px' height='120'>
<rect style='fill:white;stroke:black' x='0' y='0' width='23' height='120'/>
<rect style='fill:white;stroke:black' x='23' y='0' width='23' height='120'/>
<rect style='fill:white;stroke:black' x='46' y='0' width='23' height='120'/>
<rect style='fill:white;stroke:black' x='69' y='0' width='23' height='120'/>
<rect style='fill:white;stroke:black' x='92' y='0' width='23' height='120'/>
<rect style='fill:white;stroke:black' x='115' y='0' width='23' height='120'/>
<rect style='fill:white;stroke:black' x='138' y='0' width='23' height='120'/>
<!--  Black keys (overlap with the white keys)  -->
<rect style='fill:black;stroke:black' x='14.33333' y='0' width='13' height='80'/>
<rect style='fill:black;stroke:black' x='41.66666' y='0' width='13' height='80'/>
<rect style='fill:black;stroke:black' x='82.25' y='0' width='13' height='80'/>
<rect style='fill:black;stroke:black' x='108.25' y='0' width='13' height='80'/>
<rect style='fill:black;stroke:black' x='134.75' y='0' width='13' height='80'/>
</svg>".
