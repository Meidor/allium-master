var nodes, edges, network;
var url = "ws://10.182.5.110:8080/websocket";
var socket;
var counter = 10;
var edgeCounter = 10;


$(function () {
    if (typeof dcodeIO === 'undefined' || !dcodeIO.ProtoBuf) {
        throw(new Error("ProtoBuf.js is not present."));
    }
    // Initialize ProtoBuf.js
    var ProtoBuf = dcodeIO.ProtoBuf;
    var Wrapper = ProtoBuf.loadProtoFile("js/hrp.proto").build("EncryptedWrapper");
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
            var wrapper = new EncryptedWrapper();
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
        nodes.add([
            {id: '1', IPaddress: "127.0.0.1", port: 1337, publicKey: "secret", label: 'Node 1'},
            {id: '2', IPaddress: "127.0.0.1", port: 1337, publicKey: "secret", label: 'Node 2'},
            {id: '3', IPaddress: "127.0.0.1", port: 1337, publicKey: "secret", label: 'Node 3'},
            {id: '4', IPaddress: "127.0.0.1", port: 1337, publicKey: "secret", label: 'Node 4'},
            {id: '5', IPaddress: "127.0.0.1", port: 1337, publicKey: "secret", label: 'Node 5'}
        ]);

        edges = new vis.DataSet();
        edges.add([
            {id: '1', from: '1', to: '2', weight: '12'},
            {id: '2', from: '1', to: '3', weight: '212'},
            {id: '3', from: '2', to: '4', weight: '222'},
            {id: '4', from: '2', to: '5', weight: '999'}
        ]);

        var container = document.getElementById('network');
        var data = {
            nodes: nodes,
            edges: edges
        };
        var options = {interaction: {hover: true}};
        network = new vis.Network(container, data, options);
        network.on("selectEdge", function (data) {

            $("#edgeData").html("from node " + edges._data[data.edges[0]].from + " to node " + edges._data[data.edges[0]].to);

			$("#weight").val(edges._data[data.edges[0]].weight);
            $("#edgeId").val(edges._data[data.edges[0]].id);



			
        });

        //network.moveTo('5');
    }

    function clear() {
        nodes.clear();
    }

    drawGraph();
    initSocket();
    //setTimeout(function () {
    //    clear()
    //}, 1000);
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

        alert(JSON.stringify(toSend ));
    }

    


    function deleteEdge(edgeId) {
        edge = edges.get(edgeId);
        node = nodes.get(edge.from);

        edges.remove(edge);

        currentEdges = [];
        edges.forEach(function(edgeToAdd) {
            if(edgeToAdd.from == node.id) {
                currentEdges.push({targetNodeId: edgeToAdd.to, weight: edgeToAdd.weight});
            }
        });

        toSend = {id: node.id, IPaddress: node.IPaddress, port: node.port, publicKey: node.publicKey, edge:currentEdges };

        alert(JSON.stringify(toSend));
    }

