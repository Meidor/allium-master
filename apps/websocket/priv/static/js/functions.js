var nodes, edges, network;
var url = "ws://localhost:8080/";
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

    }

    function socketClose() {

    }

    function socketMessage(event) {
        console.log("Received message: " + event.data);
        try {
            var wrapper = Wrapper.decode(event.data);

            switch (wrapper.type) {
                case Wrapper.Type.GRAPHUPDATERESPONSE:
                    console.log("Received GraphUpdateResponse...");
                    var graphUpdateResponse = new GraphUpdateResponse().decodeDelimited(wrapper.data);
                    graphUpdateResponse.graphUpdates.forEach(function (update) {
                        var graphUpdate = new GraphUpdate().decode(update);

                        if (graphUpdate.isFullGraph) {
                            nodes.clear();
                        }

                        graphUpdate.addedNodes.forEach(function(node) {
                            addNode(node);
                        });

                        graphUpdate.deletedNodes.forEach(function(node) {
                            removeNode(node);
                        });
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
                label: node.id
            });

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
            {id: '1', label: 'Node 1'},
            {id: '2', label: 'Node 2'},
            {id: '3', label: 'Node 3'},
            {id: '4', label: 'Node 4'},
            {id: '5', label: 'Node 5'}
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
			$("#edgeDataFrom").html(JSON.stringify(edges._data[data.edges[0]].from));
			$("#edgeDataTo").html(JSON.stringify(edges._data[data.edges[0]].to));
			$("#weight").val(edges._data[data.edges[0]].weight);
			
        });

        network.moveTo('5');
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

