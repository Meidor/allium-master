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
    var Node = builder.build("Node");
    var NodeDeleteRequest = builder.build("NodeDeleteRequest");
    var NodeRegisterRequest = builder.build("NodeRegisterRequest");
    var UpdateNode = builder.build("UpdateNode");
    var GraphUpdateResponse = builder.build("GraphUpdateResponse");
    var GraphUpdate = builder.build("GraphUpdate");


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
            var message = new Wrapper({type: type, data: data});
            response = message.encodeDelimited();
            alert(JSON.stringify(response));
            socket.send(response.toArrayBuffer());
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
                    to: edge.targetNodeId,
                    weight: 1
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
        /* Voorbeelddata, to remove */
        nodes.add([
            {id: 'Node 1', IPaddress: "127.0.0.1", port: 1337, publicKey: "secret", label: 'Node 1'},
            {id: 'Node 2', IPaddress: "127.0.0.2", port: 1337, publicKey: "secret", label: 'Node 2'},
            {id: 'Node 31', IPaddress: "127.0.0.3", port: 1337, publicKey: "secret", label: 'Node 31'},
            {id: 'Node 3', IPaddress: "127.0.0.3", port: 1337, publicKey: "secret", label: 'Node 3'},
        ]);

        edges = new vis.DataSet();
        /* Voorbeelddata, to remove */
        edges.add([
            {id: 'Node 1Node 2', from: 'Node 1', to: 'Node 2', weight_from_to: '12', weight_to_from: undefined, arrows:'to'},
        ]);

        var container = document.getElementById('network');

        var data = {
            nodes: nodes,
            edges: edges
        };

        var options = {interaction: {hover: true, selectConnectedEdges: false, hoverConnectedEdges: false}};

        network = new vis.Network(container, data, options);

        network.on("selectEdge", function (data) {
                    $("#edgeData1").html("from node " + edges._data[data.edges[0]].from + " to node " + edges._data[data.edges[0]].to);
                    $("#edgeData2").html("from node " + edges._data[data.edges[0]].to + " to node " + edges._data[data.edges[0]].from);

                    $("#weight1").val(edges._data[data.edges[0]].weight_from_to);
                    $("#weight2").val(edges._data[data.edges[0]].weight_to_from);

                    $("#edgeFrom").val(edges._data[data.edges[0]].from);
                    $("#edgeTo").val(edges._data[data.edges[0]].to);

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

    drawGraph();
    initSocket();
});

    function updateEdgeWeight(edgeId, newEdgeWeight) {
        edge = edges.get(edgeId);
        node = nodes.get(edge.from);
        edge.weight = newEdgeWeight;
        edges.update(edge);

        currentEdges = [];

        edges.forEach(function(edgeToAdd) {
            if(edgeToAdd.from == node.id) {
                currentEdges.push({targetNodeId: edgeToAdd.to, weight: edgeToAdd.weight});
            }
        });
        // initSocket();
        // socketSend(UPDATENODE, toSend);
        // socketClose();
    }




    function deleteEdge(nodeId1, nodeId2) {
         console.log(nodeId1);
         console.log(nodeId2);
         console.log(nodeId1 + nodeId2);

         var edge1 = edges.get(nodeId1 + nodeId2);
         if(edge1) {
            if(edge1.weight_to_from == undefined) {
                removeEdge(nodeId1, nodeId2);
            }
            else {
                updateEdge(nodeId1, nodeId2, edge1);
            }
         } 
         else {
            var edge2 = edges.get(nodeId2 + nodeId1)
            if(edge2) {
                if(edge2.weight_to_from == undefined) {
                    removeEdge(nodeId2, nodeId1);
                }
                else {
                    updateEdge(nodeId2, nodeId1, edge);
                }
            }
         }
         createUpdateNodeMessage(nodeId1);
    }

    function updateEdge(nodeId1, nodeId2, edge) {
        edges.remove(nodeId1 + nodeId2);
        edges.add([
            {id: nodeId2 + nodeId1, from: nodeId2, to: nodeId1, weight_from_to: edge.weight_to_from, weight_to_from: undefined, arrows:'to'},
        ]);
        $("#edgeFrom").val(nodeId2);
        $("#edgeTo").val(nodeId1);
        $("#edgeData1").html("from node " + nodeId2 + " to node " + nodeId1);
        $("#edgeData2").html("from node " + nodeId1 + " to node " + nodeId2);
        $("#weight1").val(edge.weight_to_from);
        $("#weight2").val(undefined);
    }

    function removeEdge(nodeId1, nodeId2) {
        edges.remove(nodeId1 + nodeId2);
        div = document.getElementById("edit-from-edge");
        div.style.display = 'none';
    }

    function createEdgeForm(nodeId) {
        node = nodes.get(nodeId);

        $("#from").val(nodeId);

        var div = document.getElementById("add-edge-from-node");
        div.style.display = 'block';
    }

    function addEdge(fromId, toId, weight) {
        var node = nodes.get(fromId);
        var currentEdges = getEdges(fromId);
        if(checkEdge(fromId, toId)) {
            currentEdges.push({targetNodeId: toId, weight: weight});
            toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges};
            alert(JSON.stringify(toSend));
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

        var edge = edges.get(fromId + toId)
        if(edge) {
            return false;
        }

        var otherEdge = edges.get(toId + fromId)
        if(otherEdge) {
            if(otherEdge.weight_to_from != undefined) {
                return false;
            }
        }
        return true;
    }

    function createUpdateNodeMessage(Id) {
        var node = nodes.get(Id);
        alert(JSON.stringify(node));
        var currentEdges = getEdges(Id);
        toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges};

        alert(JSON.stringify(toSend));
        var newNode = new Node({id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges});
        alert("ops");
        //node = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges };
        var message = new UpdateNode({node: newNode});
        alert("peer");
        socketSend("UPDATENODE", message.encode());
    }

    function getEdges(NodeId){
        currentEdges = [];
        edges.forEach(function(edgeToAdd) {
            if(edgeToAdd.from == NodeId && edgeToAdd.weight_from_to != undefined) {
                currentEdges.push({targetNodeId: edgeToAdd.to, weight: edgeToAdd.weight_from_to});
            }
            if(edgeToAdd.to == NodeId && edgeToAdd.weight_to_from != undefined) {
                currentEdges.push({targetNodeId: edgeToAdd.from, weight: edgeToAdd.weight_to_from});
            }
        });
        return currentEdges;
    }





