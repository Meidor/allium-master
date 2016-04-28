$(function() {
    if (typeof dcodeIO === 'undefined' || !dcodeIO.ProtoBuf) {
        throw(new Error("ProtoBuf.js is not present."));
    }
    // Initialize ProtoBuf.js
    var ProtoBuf = dcodeIO.ProtoBuf;
    var Client = ProtoBuf.loadProtoFile("js/hrp.proto").build("Client");
    var firstClient = new Client();
    firstClient.setUsername("test");
    firstClient.setPublicKey("key123");

    var nodes, edges, network;

    var counter = 10;

    // convenience method to stringify a JSON object
    function toJSON(obj) {
        return JSON.stringify(obj, null, 4);
    }

    function addNode() {
        try {
            nodes.add({
                id: counter,
                label: "Node" + counter
            });
            counter++;
        }
        catch (err) {
            alert(err);
        }
    }

    function updateNode() {
        try {
            nodes.update({
                id: document.getElementById('node-id').value,
                label: document.getElementById('node-label').value
            });
        }
        catch (err) {
            alert(err);
        }
    }
    function removeNode() {
        try {
            nodes.remove({id: document.getElementById('node-id').value});
        }
        catch (err) {
            alert(err);
        }
    }

    function addEdge() {
        try {
            edges.add({
                id: document.getElementById('edge-id').value,
                from: document.getElementById('edge-from').value,
                to: document.getElementById('edge-to').value,
                weight: document.getElementById('edge-weight').value
            });

        }
        catch (err) {
            alert(err);
        }
    }
    function updateEdge() {
        try {
            edges.update({
                id: document.getElementById('edge-id').value,
                from: document.getElementById('edge-from').value,
                to: document.getElementById('edge-to').value,
                weight: document.getElementById('edge-weight').value
            });
        }
        catch (err) {
            alert(err);
        }
    }
    function removeEdge() {
        try {
            edges.remove({id: document.getElementById('edge-id').value});
        }
        catch (err) {
            alert(err);
        }
    }

    function draw() {
        // create an array with nodes
        nodes = new vis.DataSet();
        nodes.on('*', function () {

        });
        nodes.add([
            {id: '1', label: 'Node 1'},
            {id: '2', label: 'Node 2'},
            {id: '3', label: 'Node 3'},
            {id: '4', label: 'Node 4'},
            {id: '5', label: 'Node 5'}
        ]);

        // create an array with edges
        edges = new vis.DataSet();
        edges.on('*', function () {

        });
        edges.add([
            {id: '1', from: '1', to: '2', weight: '12'},
            {id: '2', from: '1', to: '3', weight: '212'},
            {id: '3', from: '2', to: '4', weight: '222'},
            {id: '4', from: '2', to: '5', weight: '999'}
        ]);

        // create a network
        var container = document.getElementById('network');
        var data = {
            nodes: nodes,
            edges: edges
        };
        var options = {interaction:{hover:true} };
        network = new vis.Network(container, data, options);
        network.on("hoverEdge", function(data) {
            //alert(toJSON(data));
            // alert(toJSON(edges.get(data.edges[0])));
        });
    }

    draw();

    setInterval(addNode, 1000);
});

