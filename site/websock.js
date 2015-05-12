function connect_to_erlang(host, port, mod){
    // console.log('connect', [host,port,mod]);
    var ws = 'ws://' + host + ':' + port + '/websocket/' + mod;
    start_session(ws);
}

function onClose(evt) {
    // change the color of the display when the socket closes
    // so we can see it closed
    // console.log('closed');
    document.body.style.backgroundColor='#ffb2b2';
    // alert("Socket closed - your erlang probably crashed");
}  
  
function onMessage(evt) {
    console.log('received', evt.data);
    var json = JSON.parse(evt.data);
    do_cmds(json);
}
  
function onError(evt) { 
    // if we get an error change the color of the display so we 
    // can see we got an error
    document.body.style.backgroundColor='orange';
}  

function send_erlang(x){
    console.log('send_erlang', x);
    send_json(x);
}

function send_json(x){
    console.log('send',x);
    send(JSON.stringify(x));
}


function send(msg) {
    websocket.send(msg);
}
  
function start_session(wsUri){
    // console.log('start_session', wsUri);
    websocket           = new WebSocket(wsUri); 
    websocket.onopen    = onOpen;
    websocket.onclose   = onClose;
    websocket.onmessage = onMessage; 
    websocket.onerror   = onError;
    return(false);
}  
    
function onOpen(evt) { 
    // console.log("connected");
}

// START:do 
function do_cmds(objs){
    console.log('do_cmds', objs);
    for(var i = 0; i < objs.length; i++){
	var o = objs[i];
	// as a safety measure we only evaluate js that is loaded
	if(eval("typeof("+o.cmd+")") == "function"){
	    eval(o.cmd + "(o)");
	} else {
	    // console.log('bad_cmd', o);
	    alert("bad_command:"+o.cmd);
	};
    };
}
// END:do


// We want the inputs to send a message when we hit CR in the input

// function make_live_inputs(){
//     $(".live_input").each(
// 	function(){
// 	    var e=$(this);
// 	    var id = e.attr('id');
//             // console.log("entry",[e,id]);
// 	    e.keyup(function(ev){
// 			if(ev.keyCode==13){
// 			    read_entry(e, id);
// 			};
// 		    });
// 	    
// 	});
// }
	
function read_entry(x, id){
    var val = x.val();
    x.val(" ");
    send_json({'entry':id, txt:val});
}
    
// browser commands

function the_id(x){
    return document.getElementById(x);
}

function append_div(o){
    var x = the_id(o.id);
    x.innerHTML += o.txt;
    // x.animate({scrollTop: x.prop("scrollHeight") }, 1000);
}

function fill_div(o){
    the_id(o.id).innerHTML = o.txt;
}

function configure(o){
    console.log('config', o);
    for(var i=0; i < o.data.length; i++){
	configure1(o.data[i][0], o.data[i][1], o.id);
    }
}

function configure1(key, val, id){
    console.log('config1', [key,val,id]);
    document.getElementById(id).setAttribute(key, val);
}

function delete_obj(o){
    var e = document.getElementById(o.id);
    e.parentNode.removeChild(e);
}

