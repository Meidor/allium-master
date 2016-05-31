var nodes = new vis.DataSet();
var edges = new vis.DataSet();
var network;
var url = "ws://localhost:8080/websocket";
var socket;
var counter = 10;
var edgeCounter = 10;
var deletedAdmin = null;

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
    var AdminRegisterRequest = builder.build("AdminRegisterRequest");
    var AdminUpdateRequest = builder.build("AdminUpdateRequest");
    var AdminDeleteRequest = builder.build("AdminDeleteRequest");
    var AdminListResponse = builder.build("AdminListResponse");
    var OnionNode = builder.build("Node");
    var UpdateNode = builder.build("UpdateNode");

    function initSocket() {
        $("#dashboard").hide();
        $("#error").hide();
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
            var wrapper = Wrapper.decodeDelimited(event.data);
            switch (wrapper.type) {
                case Wrapper.Type.GRAPHUPDATERESPONSE:
                    var graphUpdateResponse = GraphUpdateResponse.decode(wrapper.data);

                    graphUpdateResponse.graphUpdates.forEach(function (update) {
                        var graphUpdate = GraphUpdate.decode(update);

                        if (graphUpdate.isFullGraph) {
                            nodes.clear();
                        }

                        if (graphUpdate.addedNodes) {
                            graphUpdate.addedNodes.forEach(function (node) {
                                addNode(node);
                            });
                        }

                        if (graphUpdate.deletedNodes) {
                            graphUpdate.deletedNodes.forEach(function (node) {
                                removeNode(node);
                            });
                        }
                    });
                    break;
                case Wrapper.Type.ADMINLOGINRESPONSE:
                    var adminLoginResponse = AdminLoginResponse.decode(wrapper.data);
                    if (adminLoginResponse.status === AdminLoginResponse.Status.SUCCES) {
                        $("#main").hide();
                        $("#error").hide();
                        drawGraph();
                        $("#dashboard").show();
                        if(adminLoginResponse.isSuperAdmin) {
                            $("#settings-user-management").show();
                        }
                    }
                    else {
                        $("#error").show();
                    }
                    break;
                case Wrapper.Type.ADMINLISTRESPONSE:
                    var adminListResponse = AdminListResponse.decode(wrapper.data);
                    check_if_new_password(adminListResponse.newPassword);

                    if(deletedAdmin != null) {
                        if (adminListResponse.status === AdminListResponse.Status.LAST_SUPERADMIN){
                            showNotice("This is the last super admin, admin not deleted!", false);
                        }
                        else {
                            showNotice("Admin deleted!", true);
                        }
                        deletedAdmin = null;
                    }

                    switch (adminListResponse.status) {
                        case AdminListResponse.Status.SUCCES:
                            var tableContent = '';
                            adminListResponse.admins.forEach(function (admin) {
                            var superAdmin = "";
                            if(admin.superadmin) {
                                superAdmin = "<b>Yes</b>";
                            }
                            else {
                                superAdmin = "No";
                            }
                                tableContent +=
                                    '<tr class="tr">' +
                                        '<td class="td-username"><strong>' + admin.username + '</strong></td>' +
                                        '<td class="td">' + superAdmin + '</td>' +
                                        '<td class="td">' +
                                            '<input type="button" class="edit-admin padding-button" data-superadmin="' + admin.superadmin + '" data-username="' + admin.username +
                                            '" value="Edit"/>' +
                                            '<input type="button" class="delete-admin padding-button" data-username="' + admin.username +
                                            '" value="Delete"/>' +
                                        '</td>' +
                                    '</tr>';
                            });
                            $("#tbody").html(tableContent);
                            break;
                        case AdminListResponse.Status.FAILED:
                            alert("Er ging iets fout, probeer het opnieuw.");
                            break;
                        case AdminListResponse.Status.USERNAME_TAKEN:
                            alert("Deze gebruikersnaam is al bezet.");
                            break;
                        case AdminListResponse.Status.INVALID_PASSWORD:
                            alert("Het wachtwoord is niet toegestaan.");
                            break;
                        case AdminListResponse.Status.LAST_SUPERADMIN:
                            alert("Het is niet mogelijk om de laatste superadmin te verwijderen");
                            break;
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
            if (!node.edge) return;
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
                        arrows: 'from, to',
                        length: 200
                    });
                } else if (currentEdge2) {
                    edges.remove(currentEdge2.id);
                    edges.add({
                        id: currentEdge2.id,
                        from: currentEdge2.from,
                        to: currentEdge2.to,
                        weight_from_to: currentEdge1.weight_from_to,
                        weight_to_from: edge.weight,
                        arrows: 'from, to',
                        length: 200
                    });
                } else {
                    edges.add({
                        id: node.id + "-" + edge.targetNodeId,
                        from: node.id,
                        to: edge.targetNodeId,
                        weight_from_to: edge.weight,
                        weight_to_from: undefined,
                        arrows: 'to',
                        length: 200
                    });
                }
            });
        } catch (error) {
            console.log(error);
        }
    }

    function removeNode(node) {
        edges.forEach(function (edge) {
            if (edge.from == node.id) {
                if (edge.weight_to_from == undefined) {
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
            } else if (edge.to == node.id) {
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
        var container = document.getElementById('network');

        var data = {
            nodes: nodes,
            edges: edges
        };

        var options = {
            interaction: {
                hover: true,
                selectConnectedEdges: false,
                hoverConnectedEdges: false
            },
            "edges": {
                "smooth": {
                    "type": "discrete",
                    "roundness": 0
                },
                arrowStrikethrough: true
            },
            nodes: {
                shape: 'circle',
                color: 'white'
            },
            layout: {
                hierarchical: {
                    enabled: true,
                    nodeSpacing: 200,
                    sortMethod: 'hubsize'
                }
            }
        };

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

            $("#node-id").val(nodes._data[data.nodes[0]].id);

            var div = document.getElementById("edit-from-node");
            div.style.display = 'block';
            div = document.getElementById("edit-from-edge");
            div.style.display = 'none';
            var div = document.getElementById("add-edge-from-node");
            div.style.display = 'none';
        });
    }


    function createSuggestions(filter) {
        var options = "";
        if (filter.length > 0) {
            options = getOptions(filter);
        }
        document.getElementById('suggestions').innerHTML = options;
    }

    function getOptions(filter) {
        var options = "";
        nodes.forEach(function (node) {
            if (node.id.indexOf(filter) == 0) {
                options += '<option value="' + node.id + '">';
            }
        });
        return options;
    }

    function addEdge(fromId, toId, weight) {
        if (isNaN(weight) || weight < 0 || weight > 1000000) {
            return;
        }
        weight = parseInt(weight);
        var node = nodes.get(fromId);
        var currentEdges = getEdges(fromId);
        if (checkEdge(fromId, toId)) {
            currentEdges.push({targetNodeId: toId, weight: weight});
            var newNode = new OnionNode({
                id: node.id,
                IPaddress: node.IPaddress,
                port: node.port,
                publicKey: node.publicKey,
                edge: currentEdges
            });
            var message = new UpdateNode({node: newNode});
            socketSend("UPDATENODE", message.encode());
        }
    }

    function check_if_new_password(password) {
        if(password) {
            showPassword("Your new password: " + password + "<br />Double tap this message box to make it disappear.");
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
        currentEdges.forEach(function (edge) {
            if (edge.targetNodeId == nodeId2) {
                var index = currentEdges.indexOf(edge);
                if (index > -1) {
                    currentEdges.splice(index, 1);
                }
            }
        });
        var newNode = new OnionNode({
            id: node.id,
            IPaddress: node.IPaddress,
            port: node.port,
            publicKey: node.publicKey,
            edge: currentEdges
        });
        var message = new UpdateNode({node: newNode});
        socketSend("UPDATENODE", message.encode());
        if (weightBoxId == 'weight1') {
            var div = document.getElementById("edit-from-edge");
            div.style.display = 'none';
        } else {
            $("#" + weightBoxId).val(undefined);
        }
    }

    function updateEdge(nodeId1, nodeId2, weight, weightBoxId) {
        if (isNaN(weight) || weight < 0 || weight > 1000000) {
            return;
        }
        var node = nodes.get(nodeId1);
        var currentEdges = getEdges(nodeId1);
        var targetEdgeIndex = -1;
        currentEdges.forEach(function (edge) {
            if (edge.targetNodeId == nodeId2) {
                targetEdgeIndex = currentEdges.indexOf(edge);
            }
        });
        if (targetEdgeIndex > -1) {
            currentEdges.splice(targetEdgeIndex, 1);
        }
        currentEdges.push({targetNodeId: nodeId2, weight: weight});
        var newNode = new OnionNode({
            id: node.id,
            IPaddress: node.IPaddress,
            port: node.port,
            publicKey: node.publicKey,
            edge: currentEdges
        });
        var message = new UpdateNode({node: newNode});
        socketSend("UPDATENODE", message.encode());
        if (weightBoxId == 'weight1') {
            var div = document.getElementById("edit-from-edge");
            div.style.display = 'none';
        }
    }

    function checkEdge(fromId, toId) {
        if (fromId == toId) {
            return false;
        }

        var node = nodes.get(toId);
        if (!node) {
            return false;
        }

        var edge = edges.get(fromId + "-" + toId)
        if (edge) {
            return false;
        }

        var otherEdge = edges.get(toId + "-" + fromId)
        if (otherEdge) {
            if (otherEdge.weight_to_from != undefined) {
                return false;
            }
        }
        return true;
    }

    function getEdges(NodeId) {
        currentEdges = [];
        edges.forEach(function (edge) {
            if (edge.from == NodeId && edge.weight_from_to != undefined) {
                currentEdges.push({targetNodeId: edge.to, weight: edge.weight_from_to});
            }
            if (edge.to == NodeId && edge.weight_to_from != undefined) {
                currentEdges.push({targetNodeId: edge.from, weight: edge.weight_to_from});
            }
        });
        return currentEdges;
    }

    $("#finder").on('submit', function (event) {
        event.preventDefault();
        var findNode = $("#find-node");
        var search = findNode.val();
        var result = undefined;

        nodes.forEach(function (node) {
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

    $("#login").on('submit', function (event) {
        event.preventDefault();
        var username = $("#username").val();
        var password = $("#password").val();
        if (username !== undefined && password !== undefined) {
            var message = new AdminLoginRequest({username: username, password: password});
            socketSend("ADMINLOGINREQUEST", message.encode());
        }
    });

    $("#show-password").dblclick('click', function (event) {
        $("#show-password").hide();
    });

    $("#settings-user-management").on('click', function (event) {
        $("#dashboard").hide();
        $("#network").hide();
        $("#settings-user-management").hide();
        $("#settings-dashboard").show();
        $("#user-management-box").show();
        var message = new AdminListRequest();
        socketSend("ADMINLISTREQUEST", message.encode());
    });

    $("#settings-dashboard").on('click', function (event) {
        $("#settings-dashboard").hide();
        $("#user-management-box").hide();
        $("#dashboard").show();
        $("#network").show();
        showGraphDelayNotice("!! It can take some time to load the whole graph, depending on the amount of nodes in it !!");
        $("#settings-user-management").show();
    });

    $("#back-button-add").on('click', function (event) {
        $("#settings-dashboard").show();
        $("#user-management-box").show();
        $("#add-administrator-box").hide();
    });

    $("#back-button-edit").on('click', function (event) {
        $("#settings-dashboard").show();
        $("#user-management-box").show();
        $("#edit-administrator-box").hide();
    });

    $("#add-administrator-button").on('click', function (event) {
        $("#settings-dashboard").hide();
        $("#user-management-box").hide();
        $("#add-administrator-box").show();
        $("#add-username").val("");
    });

    $("#create-administrator-button").on('click', function (event) {
        var message = new AdminRegisterRequest();
        var username = $("#add-username").val();

        var correctedUsername = username.split(' ').join('_');
        if ($.trim(correctedUsername).length > 5) {
            message.username = correctedUsername;
            socketSend("ADMINREGISTERREQUEST", message.encode());
            showNotice("Admin added!", true);
            $("#settings-dashboard").show();
            $("#user-management-box").show();
            $("#add-administrator-box").hide();
        }
        else {
            showNotice("Username must contain more than 5 characters.", false);
        }
    });

    $(document).on('click', '.delete-admin', function (event) {
        var username = $(this).data('username');
        if (confirm("Are you sure you want to delete admin" + username + "?")) {
            var message = new AdminDeleteRequest();
            message.username = username;

            socketSend("ADMINDELETEREQUEST", message.encode());
            deletedAdmin = username;
        }
    });

    $("#edit-administrator-button").on('click', function (event) {
        $("#settings-dashboard").hide();
        $("#user-management-box").hide();
        $("#edit-administrator-box").show();
    });

    $(document).on('click', '.edit-admin', function (event) {
        var username = $(this).data('username');
        var superadmin = $(this).data('superadmin');
        $("#edit-username-title").html("Edit " + username);
        $("#edit-username").val(username);

        $("#settings-dashboard").hide();
        $("#user-management-box").hide();
        $("#edit-administrator-box").show();

        $("#edit-superadmin").prop("checked", superadmin);

    });

    $("#reset-password-button").on('click', function (event) {
        var message = new AdminUpdateRequest();

        message.username = $("#edit-username").val();
        message.password = "";
        message.superadmin = $("#edit-superadmin").prop('checked');
        message.resetPassword = true;
        socketSend("ADMINUPDATEREQUEST", message.encode());
    });

    $("#save-edit-admin-button").on('click', function (event) {
        password = $("#edit-password").val();
        repeatedPassword = $("#edit-password2").val();
        if(password == repeatedPassword) {
            var message = new AdminUpdateRequest();
            message.username = $("#edit-username").val();
            message.password = password;
            message.superadmin = $("#edit-superadmin").prop('checked');
            message.resetPassword = false;

            socketSend("ADMINUPDATEREQUEST", message.encode());
            showNotice("Admin edited!", true);

            $("#settings-dashboard").show();
            $("#user-management-box").show();
            $("#edit-administrator-box").hide();

            $("#edit-password2").val("");
            $("#edit-password").val("");
        }
        else {
            showNotice("Passwords do not match", false);
        }
    });

    function showNotice(message, success) {
        if (success) {
            $("#success-notice").html(message).show().delay(5000).fadeOut();
        }
        else {
            $("#error-notice").html(message).show().delay(5000).fadeOut();
        }
    }

    function showGraphDelayNotice(message) {
        $("#graph-delay-notice").html(message).show().delay(10000).fadeOut();
    }

    function showPasswordNotice(message) {
        $("#show-password-notice").html(message).show();
    }

    $("#find-node").on('keyup', function(event) {
        var to = $('#to').val();
        createSuggestions(to);
    });

    $("#weight1").on('change', function(event) {
        updateEdge(
            $('#edge-from1').val(),
            $('#edge-to1').val(),
            parseInt($('#weight1').val()),
            'weight1'
        )
    });

    $("#weight1-delete").on('click', function(event) {
        deleteEdge(
            $('#edge-from1').val(),
            $('#edge-to1').val(),
            'weight1'
        )
    });

    $("#weight2").on('change', function(event) {
        updateEdge(
            $('#edge-from2').val(),
            $('#edge-to2').val(),
            parseInt($('#weight2').val()),
            'weight2'
        )
    });

    $("#weight2-delete").on('click', function(event) {
        deleteEdge(
            $('#edge-from2').val(),
            $('#edge-to2').val(),
            'weight2'
        )
    });

    $("#add-edge").on('click', function(event) {
        var nodeId = $('#node-id').val();
        createEdgeForm(nodeId);
    });

    $("#to").on('keyup', function(event) {
        createSuggestions($('#to').val());
    });

    $("#add-edge-submit").on('click', function(event) {
        addEdge(
            $('#from').val(),
            $('#to').val(),
            parseInt($('#weight').val())
        )
    });

    initSocket();
});
