var nodes, edges, network;
var url = "ws://localhost:8080/websocket";
var socket;
var counter = 10;
var edgeCounter = 10;
var ProtoBuf = dcodeIO.ProtoBuf;
var builder = ProtoBuf.loadProtoFile("js/hrp.proto");
var Wrapper = builder.build("Wrapper");
var OnionNode = builder.build("Node");
var NodeDeleteRequest = builder.build("NodeDeleteRequest");
var NodeRegisterRequest = builder.build("NodeRegisterRequest");
var UpdateNode = builder.build("UpdateNode");
var GraphUpdateResponse = builder.build("GraphUpdateResponse");
var GraphUpdate = builder.build("GraphUpdate");

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
                    }
                    else {
                      $( "#error" ).show();
                    }
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
                label: node.IPaddress + "\n" + "port: " + node.port
            });
            if(!node.edge) return;
            node.edge.forEach(function (edge) {
                var currentEdge1 = edges.get(edge.targetNodeId + "-" + node.id);
                var currentEdge2 = edges.get(node.id + "-" + edge.targetNodeId);
                if (currentEdge1) {
                    edges.remove(currentEdge1.id);
                    edges.add({
                        id: currentEdge1.id,
                        from: currentEdge1.from,
                        to: currentEdge1.to,
                        weight_from_to: currentEdge1.weight_from_to,
                        weight_to_from: edge.weight,
                        arrows: 'from, to'
                    });
                }else if(currentEdge2){
                    edges.remove(currentEdge2.id);
                    edges.add({
                        id: currentEdge2.id,
                        from: currentEdge2.from,
                        to: currentEdge2.to,
                        weight_from_to: currentEdge1.weight_from_to,
                        weight_to_from: edge.weight,
                        arrows: 'from, to'
                    });
                } else {
                    edges.add({
                        id: node.id + "-" + edge.targetNodeId,
                        from: node.id,
                        to: edge.targetNodeId,
                        weight_from_to: edge.weight,
                        weight_to_from: undefined,
                        arrows: 'to'
                    });
                }

            });
        } catch (error) {
            console.log(error);
        }
    }

    function removeNode(node) {
        edges.forEach(function(edge) {
            if(edge.from == node.id) {
                if(edge.weight_to_from == undefined) {
                    edges.remove(edge.id);
                } else {
                    edges.remove(edge.id);
                    edges.add({
                        id: edge.to + "-" + edge.from,
                        from: edge.to,
                        to: edge.from,
                        weight_from_to: edge.weight_to_from,
                        weight_to_from: undefined,
                        arrows: 'to'
                    });
                }
            } else if(edge.to == node.id) {
                edges.remove(edge.id);
                edges.add({
                    id: edge.id,
                    from: edge.from,
                    to: edge.to,
                    weight_from_to: edge.weight_from_to,
                    weight_to_from: undefined,
                    arrows: 'to'
                });
            }
        });
        try {
            nodes.remove({
                id: node.id
            });
        } catch (error) {
            console.log(error);
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

        var options = {interaction: {hover: true, selectConnectedEdges: false, hoverConnectedEdges: false}, "edges": { "smooth": { "type": "discrete", "roundness":0}, arrowStrikethrough:true}, nodes: {shape: 'circle'}};

        network = new vis.Network(container, data, options);

        network.on("selectEdge", function (data) {
                    $("#edgeData1").html("from node " + edges._data[data.edges[0]].from + " to node " + edges._data[data.edges[0]].to);
                    $("#edgeData2").html("from node " + edges._data[data.edges[0]].to + " to node " + edges._data[data.edges[0]].from);

                    $("#weight1").val(edges._data[data.edges[0]].weight_from_to);
                    $("#weight2").val(edges._data[data.edges[0]].weight_to_from);

                    $("#edge-from1").val(edges._data[data.edges[0]].from);
                    $("#edge-to1").val(edges._data[data.edges[0]].to);

                    $("#edge-from2").val(edges._data[data.edges[0]].to);
                    $("#edge-to2").val(edges._data[data.edges[0]].from);

                    var div = document.getElementById("edit-from-edge");
                    div.style.display = 'block';
                    div = document.getElementById("edit-from-node");
                    div.style.display = 'none';
                    var div = document.getElementById("add-edge-from-node");
                    div.style.display = 'none';
                });

        network.on("selectNode", function (data) {

                    $("#nodeId").val(nodes._data[data.nodes[0]].id);

                    var div = document.getElementById("edit-from-node");
                    div.style.display = 'block';  
                    div = document.getElementById("edit-from-edge");
                    div.style.display = 'none';
                    var div = document.getElementById("add-edge-from-node");
                    div.style.display = 'none';
                });
    }

    function clear() {
        nodes.clear();
    }

    $("#finder").on('submit', function(event) {
        event.preventDefault();
        var findNode = $("#find_node");
        var search = findNode.val();

        var result = undefined;
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

    initSocket();

});

    function createSuggestions(filter) {
        var options = "";
        if(filter.length > 0) {
            options = getOptions(filter);
        }
        document.getElementById('suggestions').innerHTML = options;
    }

    function getOptions(filter) {
        var options = "";
        nodes.forEach(function(node) {
            if(node.id.indexOf(filter) == 0) {
                options += '<option value="' + node.id + '">';
            }
        });
        return options;
    }

    function addEdge(fromId, toId, weight) {
        if(weight < 0 || weight > 1000000) {
            return;
        }
        weight = parseInt(weight);
        var node = nodes.get(fromId);
        var currentEdges = getEdges(fromId);
        if(checkEdge(fromId, toId)) {
            currentEdges.push({targetNodeId: toId, weight: weight});
            var newNode = new OnionNode({id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges});
            var message = new UpdateNode({node: newNode});
            socketSend("UPDATENODE", message.encode());
        }
    }

    function createEdgeForm(nodeId) {
        $("#from").val(nodeId);
        var div = document.getElementById("add-edge-from-node");
        div.style.display = 'block';
    }

    function deleteEdge(nodeId1, nodeId2, weightBoxId) {
        var node = nodes.get(nodeId1);
        var currentEdges = getEdges(nodeId1);
        currentEdges.forEach(function(edge){
            if(edge.targetNodeId == nodeId2) {
                var index = currentEdges.indexOf(edge);
                if (index > -1) {
                    currentEdges.splice(index, 1);
                }
            }
        });
        var newNode = new OnionNode({id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges});
        var message = new UpdateNode({node: newNode});
        socketSend("UPDATENODE", message.encode());
        if(weightBoxId == 'weight1') {
            var div = document.getElementById("edit-from-edge");
            div.style.display = 'none';
        } else {
            $("#" + weightBoxId).val(undefined);
        }
    }

    function updateEdge(nodeId1, nodeId2, weight, weightBoxId) {
        if(weight < 0 || weight > 1000000) {
            return;
        }
        weight = parseInt(weight);
        var node = nodes.get(nodeId1);
        var currentEdges = getEdges(nodeId1);
        var targetEdgeIndex = -1;
        currentEdges.forEach(function(edge){
            if(edge.targetNodeId == nodeId2) {
                targetEdgeIndex = currentEdges.indexOf(edge);
            }
        });
        if (targetEdgeIndex > -1) {
            currentEdges.splice(targetEdgeIndex, 1);
        }
        currentEdges.push({targetNodeId: nodeId2, weight: weight});
        var newNode = new OnionNode({id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges});
        var message = new UpdateNode({node: newNode});
        socketSend("UPDATENODE", message.encode());
        if(weightBoxId == 'weight1') {
            var div = document.getElementById("edit-from-edge");
            div.style.display = 'none';
        }
    }

    function checkEdge (fromId, toId) {
        if(fromId == toId) {
            return false;
        }

        var node = nodes.get(toId);
        if(!node) {
            return false;
        }

        var edge = edges.get(fromId + "-" + toId)
        if(edge) {
            return false;
        }

        var otherEdge = edges.get(toId + "-" + fromId)
        if(otherEdge) {
            if(otherEdge.weight_to_from != undefined) {
                return false;
            }
        }
        return true;
    }

    function getEdges(NodeId){
        currentEdges = [];
        edges.forEach(function(edge) {
            if(edge.from == NodeId && edge.weight_from_to != undefined) {
                currentEdges.push({targetNodeId: edge.to, weight: edge.weight_from_to});
            }
            if(edge.to == NodeId && edge.weight_to_from != undefined) {
                currentEdges.push({targetNodeId: edge.from, weight: edge.weight_to_from});
            }
        });
        return currentEdges;
    }

    function socketSend(type, data) {
        if (socket.readyState == WebSocket.OPEN) {
            var message = new Wrapper({type: type, data: data});
            var response = message.encodeDelimited();
            socket.send(response.toArrayBuffer());
        } else {
            console.log("Not connected while sending: " + data);
        }
    }
