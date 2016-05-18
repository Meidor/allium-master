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
    var Wrapper = ProtoBuf.loadProtoFile("js/hrp.proto").build("Wrapper");
    var GraphUpdateResponse = ProtoBuf.loadProtoFile("js/hrp.proto").build("GraphUpdateResponse");
    var GraphUpdate = ProtoBuf.loadProtoFile("js/hrp.proto").build("GraphUpdate");

    function initSocket() {
        console.log("Initializing socket");
        socket = new WebSocket(url);
        socket.binaryType = "arraybuffer";

        socket.onopen = socketOpen;
        socket.onclose = socketClose;
        socket.onmessage = socketMessage;
    }

    function socketSend(type, data) {
        if (socket.readyState == WebSocket.OPEN) {
            var wrapper = new Wrapper();
            wrapper.setType(type);
            wrapper.setData(data);
            socket.send(wrapper.toArrayBuffer());
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
                    console.log("Received GraphUpdateResponse...");
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

            var response = network.focus(username, password);
			if (response) {
				window.location = "index.html";
			}
			else {
				alert("You have submitted the wrong user credentials.");
			}
        }
    });

    drawGraph();
    initSocket();
});
    



