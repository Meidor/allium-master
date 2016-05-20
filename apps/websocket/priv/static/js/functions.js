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

                    var div = document.getElementById("eddit from edge");
                    div.style.display = 'block';
                    div = document.getElementById("eddit from node");
                    div.style.display = 'none';
                    var div = document.getElementById("add edge from node");
                    div.style.display = 'none';
                });

        network.on("selectNode", function (data) {

                    $("#nodeId").val(nodes._data[data.nodes[0]].id);

                    var div = document.getElementById("eddit from node");
                    div.style.display = 'block';  
                    div = document.getElementById("eddit from edge");
                    div.style.display = 'none';
                    var div = document.getElementById("add edge from node");
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

        toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges };

        alert(JSON.stringify(toSend));

        // initSocket();
        // socketSend(UPDATENODE, toSend);
        // socketClose();
    }




    function deleteEdge(nodeId1, nodeId2) {
         console.log(nodeId1);
         console.log(nodeId2);
         console.log(nodeId1 + nodeId2);

         var edge1 = edges.get(nodeId1 + nodeId2)
         if(edge1) {
            if(edge1.weight_to_from == undefined) {
                console.log('case zelf source enkel');
                edges.remove(nodeId1 + nodeId2);
            }
            else {
                console.log('case zelf source dubbel');
                edges.remove(nodeId1 + nodeId2);
                edges.add([
                    // {id: edge1.id, from: edge1.from, to: edge1.to, weight_from_to: undefined, weight_to_from: edge1.weight_to_from, arrows:'from'},
                    {id: nodeId2 + nodeId1, from: nodeId2, to: nodeId1, weight_from_to: edge1.weight_to_from, weight_to_from: undefined, arrows:'to'},
                ]);
            }
         } 
         else {
            var edge2 = edges.get(nodeId2 + nodeId1)
            if(edge2) {
                if(edge2.weight_to_from == undefined) {
                    console.log('case ander source enkel');
                    edges.remove(nodeId2 + nodeId1);
                }
                else {
                    console.log('case ander source dubbel');
                    edges.remove(nodeId2 + nodeId1);
                    edges.add([
                        {id: edge2.id, from: edge2.from, to: edge2.to, weight_from_to: edge2.weight_from_to, weight_to_from: undefined, arrows:'to'},
                    ]);
                    $("#weight2").val(undefined);
                }
            }
         }
        // edge = edges.get(edgeId);
        // console.log(edgeId);
        // node = nodes.get(edge.from);
        // edges.remove(edge);

        // currentEdges = getEdges(node.id);

        // toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges };

        // alert(JSON.stringify(toSend));

        // initSocket();
        // socketSend(UPDATENODE, toSend);
        // socketClose();
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

    function createEdgeForm(nodeId) {
        node = nodes.get(nodeId);

        $("#from").val(nodeId);

        var div = document.getElementById("add edge from node");
        div.style.display = 'block';
    }

    function addEdge(fromId, toId, weight) {
        if(checkEdge(fromId, toId)) {

            if(edges.get(toId + fromId)) {
                previousEdge = edges.get(toId + fromId);
                edges.remove(previousEdge.id);
                edges.add([
                            {id: previousEdge.id, from: previousEdge.from, to: previousEdge.to, weight_from_to: previousEdge.weight_from_to, weight_to_from: weight, arrows:'from to'},
                           ]);
            }
            else {
                edges.add([
                            {id: fromId + toId, from: fromId, to: toId, weight_from_to: weight, weight_to_from: undefined, arrows:'to'},
                           ]);
            }
            createAddEdgeMessage(fromId);
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
            if(edge.weight_from_to != undefined) {
                return false;
            }
        }

        var otherEdge = edges.get(toId + fromId)
        if(otherEdge) {
            if(otherEdge.weight_to_from != undefined) {
                return false;
            }
        }
        return true;
    }

    function createAddEdgeMessage(Id) {
        var node = nodes.get(Id);
        var currentEdges = getEdges(Id);

        toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges };

        alert(JSON.stringify(toSend));

    }






