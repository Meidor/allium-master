var nodes, edges, network;
var url = "ws://localhost:8080/websocket";
var socket;
var counter = 10;
var edgeCounter = 10;

$(function () {
    if (typeof dcodeIO === 'undefined' || !dcodeIO.ProtoBuf) {
        throw(new Error("ProtoBuf.js is not present."));
    }

    // Initialize ProtoBuf.js
	var ProtoBuf = dcodeIO.ProtoBuf;
    var builder = ProtoBuf.loadProtoFile("js/hrp.proto");
    var Wrapper = builder.build("Wrapper");
    var NodeDeleteRequest = builder.build("NodeDeleteRequest");
    var NodeRegisterRequest = builder.build("NodeRegisterRequest");
    var GraphUpdateResponse = builder.build("GraphUpdateResponse");
    var GraphUpdate = builder.build("GraphUpdate");
	var AdminLoginRequest = builder.build("AdminLoginRequest");
	var AdminLoginResponse = builder.build("AdminLoginResponse");
	var AdminListRequest = builder.build("AdminListRequest");
	var AdminListResponse = builder.build("AdminListResponse");

    function initSocket() {
        $( "#dashboard" ).hide();
        $( "#error" ).hide();
        console.log("Initializing socket");
        socket = new WebSocket(url);
        socket.binaryType = "arraybuffer";

        socket.onopen = socketOpen;
        socket.onclose = socketClose;
        socket.onmessage = socketMessage;
    }

	function socketSend(type, data) {
        if (socket.readyState == WebSocket.OPEN) {
            var message = new Wrapper({type: type, data: data});
            socket.send(message.encodeDelimited().toArrayBuffer());
        } else {
            console.log("Not connected while sending: " + data);
        }
    }

    function socketOpen() {
        console.log("WebSocket connection established");
    }

    function socketClose() {
        console.log("WebSocket connection closed");
    }

    function socketMessage(event) {
        console.log("Received message: " + event.data);
        try {
            var wrapper = Wrapper.decode(event.data);
            switch (wrapper.type) {
                case Wrapper.Type.GRAPHUPDATERESPONSE:
                    var graphUpdateResponse = GraphUpdateResponse.decode(wrapper.data);

                    graphUpdateResponse.graphUpdates.forEach(function (update) {
                        var graphUpdate = GraphUpdate.decode(update);

                        if (graphUpdate.isFullGraph) {
                            nodes.clear();
                        }
                        if(graphUpdate.addedNodes){
                            graphUpdate.addedNodes.forEach(function(node) {
                                addNode(node);
                            });
                        }

                        if(graphUpdate.deletedNodes){
                            graphUpdate.deletedNodes.forEach(function(node) {
                                removeNode(node);
                            });
                        }
                    });
                    break;
                case Wrapper.Type.ADMINLOGINRESPONSE:
                    var adminLoginResponse = AdminLoginResponse.decode(wrapper.data);
                    if(adminLoginResponse.status === AdminLoginResponse.Status.SUCCES) {
                      $( "#main" ).hide();
                      $( "#error" ).hide();
                      drawGraph();
                      $( "#dashboard" ).show();
                      $( "#settings-user-management" ).show();
                    }
                    else {
                      $( "#error" ).show();
                    }
                    break;
                case Wrapper.Type.ADMINLISTRESPONSE:
                    var adminListResponse = AdminListResponse.decode(wrapper.data);
                    adminListResponse.forEach(function(admin) {
                        $( "#tbody" ).innerHTML = '<tr>' +
                                                    '<td class="' + admin.username + '"><strong>Administrator</strong></td>' +
                                                    '<td>' + admin.superadmin + '</td>' +
                                                    '<td><input type="button" id="edit-administrator-button" class="padding-button"' +
                                                    'value="Edit"/> | <input type="submit" id="delete-admin-' + admin.username +
                                                    '" class="padding-button" value="Delete-' + admin.username + '"/></td>' +
                                                  '</tr>';
                    });
                    break;
            }
        } catch (error) {
            console.log("Error while receiving message: " + error);
        }
    }

    function addNode(node) {
        try {
            nodes.add({
                id: node.id,
                IPaddress: node.IPaddress,
                port: node.port,
                publicKey: node.publicKey,
                label: node.id
            });
            if(!node.edges) return;
            
            node.edges.forEach(function (edge) {
                edges.add({
                    id: node.id + edge.targetNodeId,
                    from: node.id,
                    to: edge.targetNodeId
                });
            });
        } catch (error) {
            alert(error);
        }
    }

    function removeNode(node) {
        try {
            nodes.remove({
                id: node.id
            });
        } catch (error) {
            alert(error);
        }
    }

    function drawGraph() {
        nodes = new vis.DataSet();
        edges = new vis.DataSet();

        var container = document.getElementById('network');

        var data = {
            nodes: nodes,
            edges: edges
        };

        var options = {interaction: {hover: true}};

        network = new vis.Network(container, data, options);
    }

    function clear() {
        nodes.clear();
    }

    $("#finder").on('submit', function(event) {
        event.preventDefault();
        var findNode = $("#find_node");
        var search = findNode.val();

        var result;
        nodes.forEach(function(node) {
            if (node.id.lastIndexOf(search, 0) === 0) {
                result = node;
            }
        });

        if (result !== undefined) {
            network.focus(result.id);
            network.selectNodes([result.id]);
        }

        findNode.val("");
    });

    $("#login").on('submit', function(event) {
        event.preventDefault();
        var username = $("#username").val();
        var password = $("#password").val();
        if (username !== undefined && password !== undefined) {
			var message = new AdminLoginRequest({username : username, password: password});
            socketSend("ADMINLOGINREQUEST", message.encode());
        }
    });

   $("#settings-user-management").on('click', function(event) {
        $( "#dashboard" ).hide();
        $( "#settings-user-management" ).hide();
        $( "#settings-dashboard" ).show();
        $( "#user-management-box" ).show();
        var message = new AdminListRequest();
        socketSend("ADMINLISTREQUEST", message.encode());
    });

   $("#settings-dashboard").on('click', function(event) {
        $( "#settings-dashboard" ).hide();
        $( "#user-management-box" ).hide();
        $( "#dashboard" ).show();
        $( "#settings-user-management" ).show();
    });

    $("#back-button-add").on('click', function(event) {
        $( "#settings-dashboard" ).show();
        $( "#user-management-box" ).show();
        $( "#add-administrator-box" ).hide();
    });

    $("#back-button-edit").on('click', function(event) {
            $( "#settings-dashboard" ).show();
            $( "#user-management-box" ).show();
            $( "#edit-administrator-box" ).hide();
        });

    $("#add-administrator-button").on('click', function(event) {
        $( "#settings-dashboard" ).hide();
        $( "#user-management-box" ).hide();
        $( "#add-administrator-box" ).show();
    });

    $("#edit-administrator-button").on('click', function(event) {
        $( "#settings-dashboard" ).hide();
        $( "#user-management-box" ).hide();
        $( "#edit-administrator-box" ).show();
    });

    initSocket();
});
    



